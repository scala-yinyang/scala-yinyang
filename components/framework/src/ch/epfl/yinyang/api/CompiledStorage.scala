package ch.epfl.yinyang.runtime

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable.{ DoubleLinkedList, HashSet }
import scala.ref.WeakReference

object YYStorage {
  /* DESIGN of recompilation and stabilization of optional variables:
   * 
   * Each DSL instance has several compilation variables, each with a guard
   * function and marked as optional or required. Optional variables are marked
   * as stable or unstable (producing generic code) at runtime. For unstable
   * variables, a count indicates how many matching values have been seen. Two
   * values "match" if both weak references are still valid and they're
   * equivalent w.r.t. the guard.
   *
   * DSL instances are compiled to variants, which are characterized by the
   * values they were compiled for, and the stability decisions for the
   * optional variables. Every check with new values returns either an existing
   * variant or causes recompilation. A variant "matches" if all required and
   * stable variables match, the unstable variables are ignored. The size of
   * the cache is defined by the codeCacheSize attribute of the DSL instance,
   * using an LRU eviction policy. The returned variant is always moved to the
   * head.
   *
   * When no variants have been compiled yet, optional variables are designated
   * as stable or unstable depending on the optionalInitiallyStable attribute
   * of the DSL instance.
   *
   * When no matching variants are available, a new variant is compiled based
   * on the previous head variant. If an optional variable matches the previous
   * value, it inherits the stability and count, but possibly gets promoted to
   * a stable variable if it was unstable before but has reached the DSL
   * instance attribute minimumCountToStabilize. If it doesn't match, it is
   * marked as unstable with a reset count.
   *
   * When the head variant matches, the counts of matching unstable variables
   * are incremented, and the new value is stored with a reset count if an
   * unstable variable doesn't match.
   *
   * When a variant from the tail matches, it can inherit values and counts
   * from the previous head. So for each unstable variable, we have three
   * tuples (value, count) to consider: the previous head, the chosen variant
   * and the new value being checked. The decision tree looks as follows:
   * if (new matches prev) (prev_val, prev_count + 1)
   * else if (new matches chosen) (keep chosen_val, chosen_count + 1)
   * else (new_val, 1)
   *
   * The result of this algorithm is that we keep traces of both per-variant
   * history to detect correlation and recent history to adapt to the current
   * situation. Priority is given to absolute counts since we deem that
   * locality is more important than co-locality.
   */

  // Debugging information
  private var runtimeCompileCount = 0
  private var compileTimeCompileCount = 0
  private var alreadyCountedCompileTimeUIDs: List[Long] = Nil
  private var checkCount = 0
  private var checkTime = 0L
  private var hashLookupTime = 0L
  @inline
  final def getCheckTimeCount() = (checkTime, checkCount)
  @inline
  final def getHashLookupTime() = hashLookupTime
  @inline
  final def getRuntimeCompileCount() = runtimeCompileCount
  @inline
  final def getCompileTimeCompileCount() = compileTimeCompileCount
  @inline
  final def incrementCompileTimeCompileCount(uID: Long) = {
    if (!alreadyCountedCompileTimeUIDs.contains(uID)) {
      compileTimeCompileCount += 1
      alreadyCountedCompileTimeUIDs ::= uID
    }
  }

  type GuardFun = (Any, Any) => Boolean
  private val REQUIRED_ID = -1
  /**
   * Each DSL program is represented by a singleton instance of the transformed
   * DSL class and attributes controlling cache behavior and stability. It also
   * stores the guards for all compilation variables, the holeIds for optional
   * variables resp. REQUIRED_ID for required variables and the variant cache.
   */
  final private case class DSLInstance(classInstance: Any, optionalInitiallyStable: Boolean, codeCacheSize: Int,
                                       minimumCountToStabilize: Int, guards: Array[GuardFun], optionalHoleIds: Array[Int],
                                       variantCache: DoubleLinkedList[ProgramVariant])

  /** DSL instances are stored and retrieved by their UID. */
  final private val DSLs = new ConcurrentHashMap[Long, DSLInstance]

  /**
   * A program variant in the cache consists of the compiled function, to be
   * cast to the right type. Additionally, it stores the values it was compiled
   * for, so they can be checked by the guard at the next possible execution.
   * Required variables have a count of REQUIRED_COUNT, stable ones
   * STABLE_COUNT and unstable ones use strictly positive values, indicating
   * how many consecutive times the value has stayed the same w.r. to the guard
   * function.
   */
  final private case class Reference(ref: WeakReference[AnyRef], var count: Int)
  final private case class ProgramVariant(val function: Any, val refs: Array[Reference])
  @inline
  final private def isGuarded(count: Int) = count <= 0
  private val REQUIRED_COUNT = 0
  private val STABLE_COUNT = -1
  private val UNSTABLE_COUNT = 1

  @inline
  final def lookup[Ret](id: Long, dsl: => Ret, guardFuns: => Array[GuardFun],
                        optional: Array[Int], optionalInitiallyStable: Boolean,
                        codeCacheSize: Int, minimumCountToStabilize: Int): Ret = {
    val start = System.nanoTime()
    val dslInstance = DSLs.get(id)
    hashLookupTime += System.nanoTime() - start

    (if (dslInstance == null) {
      // Null check because we don't want to eval dsl and guardFuns unless necessary.
      val d = DSLInstance(dsl, optionalInitiallyStable, codeCacheSize,
        minimumCountToStabilize, guardFuns, optional, new DoubleLinkedList[ProgramVariant]())
      val start = System.nanoTime()
      DSLs.putIfAbsent(id, d)
      hashLookupTime += System.nanoTime() - start
      d
    } else dslInstance).classInstance.asInstanceOf[Ret]
  }

  /**
   * Compiles a new variant using the stability information from the refCounts
   * and stores it using an LRU cache eviction policy.
   */
  @inline
  final private def createAndStoreVariant(refs: Array[Any], refCounts: Array[Int],
                                          recompile: Set[Int] => Any, dsl: DSLInstance): ProgramVariant = {
    runtimeCompileCount += 1

    val unstableSetBuilder = scala.collection.immutable.HashSet.newBuilder[Int]
    val holeIds = dsl.optionalHoleIds
    assert(refs.size == refCounts.size && refs.size == holeIds.size) // sanity check
    for (i <- 0 until refCounts.size) {
      if (!isGuarded(refCounts(i))) {
        unstableSetBuilder += holeIds(i)
      }
    }

    val refArray = new Array[Reference](refs.size)
    for (i <- 0 until refs.size) {
      refArray(i) = Reference(new WeakReference[AnyRef](refs(i).asInstanceOf[AnyRef]), refCounts(i))
    }

    val variant = new ProgramVariant(recompile(unstableSetBuilder.result), refArray)

    var cache = dsl.variantCache
    cache.synchronized {
      if (cache.isEmpty) {
        cache.next = DoubleLinkedList()
        cache.next.prev = cache
      } else {
        cache.insert(DoubleLinkedList(cache.elem))
      }
      cache.elem = variant

      var i = 1
      while (!cache.isEmpty) {
        if (i > dsl.codeCacheSize) {
          cache.remove // remove current node from linked list
        }
        i += 1
        cache = cache.next // current node still points into list
      }
    }
    variant
  }

  @inline
  final private def compileInitialVariant(refs: Array[Any], recompile: Set[Int] => Any,
                                          dsl: DSLInstance): ProgramVariant = {
    val optionalCount = if (dsl.optionalInitiallyStable) STABLE_COUNT else UNSTABLE_COUNT
    val holeIds = dsl.optionalHoleIds

    val refCounts = new Array[Int](holeIds.size)
    for (i <- 0 until holeIds.size) {
      refCounts(i) = if (holeIds(i) == REQUIRED_ID) REQUIRED_COUNT else optionalCount
    }

    createAndStoreVariant(refs, refCounts, recompile, dsl)
  }

  @inline
  final private def recompileVariant(refs: Array[Any], recompile: Set[Int] => Any,
                                     dsl: DSLInstance, headVariant: ProgramVariant): ProgramVariant = {
    val minimumCountToStabilize = dsl.minimumCountToStabilize
    val headRefs = headVariant.refs
    val guards = dsl.guards

    val refCounts = new Array[Int](headRefs.size)
    for (i <- 0 until headRefs.size) {
      val refI = headRefs(i) // Get a particular Reference object
      refCounts(i) = if (refI.count == REQUIRED_COUNT) {
        REQUIRED_COUNT
      } else {
        refI.ref.get match {
          case Some(ref1) if guards(i)(ref1, refs(i)) =>
            if (refI.count == STABLE_COUNT || (refI.count >= minimumCountToStabilize))
              STABLE_COUNT
            else
              refI.count + 1 // Always use the newest count
          case _ => UNSTABLE_COUNT
        }
      }
    }

    createAndStoreVariant(refs, refCounts, recompile, dsl)
  }

  /**
   * Computes the next unstable counts of the chosen existing variant. If the
   * variant was just promoted to the head, the previous head needs to be
   * provided. The unstable variables of the existing variant inherit the
   * values and counts of the previous one if they match the new values.
   * Otherwise, the count is either incremented if the new value matches the
   * existing ones, or the new value is stored with a reset count.
   */
  @inline
  final private def updateVariant(variant: ProgramVariant, refs: Array[Any], dsl: DSLInstance,
                                  previousHead: Option[ProgramVariant]): ProgramVariant = {
    val variantRefs = variant.refs
    val guards = dsl.guards
    val inherited = Array.fill(variantRefs.size)(false)

    // If the variant was just promoted to the head, the unstable variables
    // can inherit counts and values from the previous head.
    previousHead.map({ prevVariant =>
      val prevVariantRefs = prevVariant.refs
      for (i <- 0 until variantRefs.size) {
        val variantRef = variantRefs(i)
        val prevRef = prevVariantRefs(i)
        // Unstable in both the existing and the previous variant.
        if (!isGuarded(variantRef.count) && !isGuarded(prevRef.count)) {
          prevRef.ref.get match {
            case Some(prev) if guards(i)(prev, refs(i)) =>
              // Previous value not GC'ed and matches new value, so inherit.
              prevRef.count += 1
              variantRefs(i) = prevRef
              inherited(i) = true
            case _ =>
          }
        }
      }
    })

    // If no count was inherited (either no previous head, or doesn't match
    // new), check new against the chosen existing variant.
    for (i <- 0 until variantRefs.size) {
      val variantRef = variantRefs(i)
      if (!isGuarded(variantRef.count) && !inherited(i)) {
        variantRef.ref.get match {
          case Some(varRef) if guards(i)(varRef, refs(i)) =>
            // Match, increment count.
            variantRef.count += 1
          case _ =>
            // Doesn't match, store the new value and reset the count.
            variantRefs(i) = Reference(new WeakReference[AnyRef](refs(i).asInstanceOf[AnyRef]), UNSTABLE_COUNT)
        }
      }
    }

    variant // For chaining
  }

  @inline
  final def check[Ret](id: Long, refs: Array[Any], recompile: Set[Int] => Any): Ret = {
    val startTime = System.nanoTime()

    val dsl = DSLs.get(id)
    hashLookupTime += System.nanoTime() - startTime

    val cache = dsl.variantCache.synchronized {
      dsl.variantCache.toList
    }
    val ret = (cache match {
      case Nil => compileInitialVariant(refs, recompile, dsl)
      case head :: _ => checkHead(refs, recompile, dsl, head) match {
        case Right(variant) => variant
        case Left(check)    => checkAll(refs, recompile, dsl, cache, check)
      }
    }).function.asInstanceOf[Ret]

    checkCount += 1
    checkTime += System.nanoTime() - startTime
    ret
  }

  private val GUARDS_OK = 1
  private val GUARDS_FAILED = 0
  private val GCED = -1
  @inline
  final private def reqAndStableGuardsOk(variantRefs: Array[Reference], refs: Array[Any],
                                         guards: Array[GuardFun]): Int = {
    for (i <- 0 until refs.length) {
      val ref = variantRefs(i)
      if (isGuarded(ref.count)) {
        ref.ref.get match {
          case Some(oldRef) => if (!guards(i)(oldRef, refs(i))) return GUARDS_FAILED
          case None         => return GCED
        }
      }
    }
    GUARDS_OK
  }

  @inline
  final private def checkHead(refs: Array[Any], recompile: Set[Int] => Any,
                              dsl: DSLInstance, headVariant: ProgramVariant): Either[Int, ProgramVariant] = {
    // Check whether all guarded values are OK
    val check = reqAndStableGuardsOk(headVariant.refs, refs, dsl.guards)
    if (check == GUARDS_OK) {
      updateVariant(headVariant, refs, dsl, None)
      Right(headVariant)
    } else {
      Left(check)
    }
  }

  @inline
  final private def checkAll(refs: Array[Any], recompile: Set[Int] => Any,
                             dsl: DSLInstance, variants: Seq[ProgramVariant],
                             headCheck: Int): ProgramVariant = {
    val GCedBuilder = HashSet.newBuilder[ProgramVariant]
    var maxVariant: Option[(ProgramVariant, Int)] = None

    if (headCheck == GCED)
      GCedBuilder += variants.head

    variants.tail.foreach({ variant =>
      val res = reqAndStableGuardsOk(variant.refs, refs, dsl.guards)
      if (res == GCED) {
        GCedBuilder += variant
      } else if (res == GUARDS_OK) {
        var stableCount = 0
        val variantRefs = variant.refs
        for (i <- 0 until variantRefs.size) {
          if (variantRefs(i).count == STABLE_COUNT)
            stableCount += 1
        }
        maxVariant match {
          case Some((otherVariant, otherCount)) if (stableCount <= otherCount) =>
          case _ => maxVariant = Some((variant, stableCount))
        }
      }
    })

    // Use head even if it has been GCed, because it still contains the
    // up-to-date counts. Head always exists.
    val (variant, needsInsert) = maxVariant match {
      case None =>
        (recompileVariant(refs, recompile, dsl, variants.head), false)
      case Some((variant, _)) =>

        (updateVariant(variant, refs, dsl, Some(variants.head)), true)
    }
    val variantsToRemove = GCedBuilder.result

    // Update cache
    if (!variantsToRemove.isEmpty || needsInsert) {
      val cache = dsl.variantCache
      cache.synchronized {
        if (needsInsert) {
          cache.insert(DoubleLinkedList(cache.head))
          cache.elem = variant
          variantsToRemove += variant // Variant inserted at head, remove from tail.
        }
        var c = cache.next // Ignore head since that's the chosen variant
        while (!c.isEmpty && !variantsToRemove.isEmpty) {
          if (variantsToRemove.contains(c.elem)) {
            c.remove
            variantsToRemove -= c.elem
          }
          c = c.next
        }
      }
    }

    variant
  }
}
