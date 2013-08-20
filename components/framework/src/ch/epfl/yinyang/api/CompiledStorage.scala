package ch.epfl.yinyang.runtime

import java.util.concurrent.ConcurrentHashMap
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

  /** Each DSL program is represented by a singleton instance of the transformed DSL class. */
  // Promotion currently only happens on recompilation. TODO periodically check unstable?
  final private case class DSLInstance(classCode: Any, optionalInitiallyStable: Boolean, codeCacheSize: Int,
                                       minimumCountToStabilize: Int)

  /** DSL instances are stored and retrieved by their UID. */
  final private val DSLInstances = new ConcurrentHashMap[Long, DSLInstance]

  type GuardFun = (Any, Any) => Boolean
  /**
   * A guard consists of a guard function and an Int holding the holeId if it is
   * an optional variable, and REQUIRED_ID otherwise.
   */
  final private case class GuardFunction(fun: GuardFun, optionalHoleId: Int)
  private val REQUIRED_ID = -1

  /** For each DSL instance, there is a GuardFunction for each compilation variable. */
  final private val guardFunctions = new ConcurrentHashMap[Long, Seq[GuardFunction]]()

  /**
   * A program variant in the cache consists of the compiled function, to be
   * cast to the right type. Additionally, it stores the values it was compiled
   * for, so they can be checked by the guard at the next possible execution.
   * Required variables have a count of REQUIRED_COUNT, stable ones
   * STABLE_COUNT and unstable ones use strictly positive values, indicating
   * how many consecutive times the value has stayed the same w.r. to the guard
   * function.
   */
  final private case class ProgramVariant(val function: Any, val refs: Array[WeakReference[AnyRef]], val refCounts: Array[Int])
  @inline
  final private def isGuarded(count: Int) = count <= 0
  private val REQUIRED_COUNT = 0
  private val STABLE_COUNT = -1
  private val UNSTABLE_COUNT = 1

  /** For each DSL instance, a maximum of dslInstance.codeCacheSize compiled variants are cached. */
  final private val programVariants = new ConcurrentHashMap[Long, Seq[ProgramVariant]]()

  @inline
  final def lookup[Ret](id: Long, dsl: => Ret, guardFuns: => Seq[GuardFun],
                        optional: Seq[Int], optionalInitiallyStable: Boolean,
                        codeCacheSize: Int, minimumCountToStabilize: Int): Ret = {
    val dslInstance = DSLInstances.get(id)

    (if (dslInstance == null) {
      DSLInstances.put(id, DSLInstance(dsl, optionalInitiallyStable, codeCacheSize, minimumCountToStabilize))
      guardFunctions.put(id, guardFuns zip optional map (g => new GuardFunction(g._1, g._2)))
      dsl
    } else dslInstance.classCode).asInstanceOf[Ret]
  }

  /**
   * Compiles a new variant using the stability information from the refCounts
   * and stores it using an LRU cache eviction policy.
   */
  @inline
  final private def createAndStoreVariant(id: Long, refs: Seq[Any], refCounts: Array[Int],
                                          recompile: Set[Int] => Any, guardFuns: Seq[GuardFunction]): ProgramVariant = {
    runtimeCompileCount += 1

    val unstableSet = refCounts zip guardFuns collect {
      case (count, GuardFunction(_, id)) if (!isGuarded(count)) => assert(id != REQUIRED_ID); id
    } toSet
    val variant = new ProgramVariant(recompile(unstableSet),
      refs.asInstanceOf[Seq[AnyRef]].map(new WeakReference[AnyRef](_)).toArray, refCounts)

    programVariants.put(id, programVariants.get(id) match {
      case null => Seq(variant)
      case vs   => variant +: (vs.take(DSLInstances.get(id).codeCacheSize - 1))
    })
    variant
  }

  @inline
  final private def compileInitialVariant(id: Long, refs: Seq[Any], recompile: Set[Int] => Any,
                                          guardFuns: Seq[GuardFunction]): ProgramVariant = {
    val optionalCount = if (DSLInstances.get(id).optionalInitiallyStable) STABLE_COUNT else UNSTABLE_COUNT
    val refCounts = guardFuns map {
      case GuardFunction(_, REQUIRED_ID) => REQUIRED_COUNT
      case _                             => optionalCount
    }
    createAndStoreVariant(id, refs, refCounts.toArray, recompile, guardFuns)
  }

  @inline
  final private def recompileVariant(id: Long, refs: Seq[Any], recompile: Set[Int] => Any,
                                     guardFuns: Seq[GuardFunction], headVariant: ProgramVariant): ProgramVariant = {
    val minimumCountToStabilize = DSLInstances.get(id).minimumCountToStabilize
    val refCounts = headVariant.refCounts.zip(headVariant.refs.zip(refs).zip(guardFuns)) map {
      case (count, _) if count == REQUIRED_COUNT => REQUIRED_COUNT
      case (count, ((weakRef1, ref2), eq)) if (count == STABLE_COUNT || count >= minimumCountToStabilize) =>
        weakRef1.get match {
          case Some(ref1) if (eq.fun(ref1, ref2)) => STABLE_COUNT
          case _                                  => UNSTABLE_COUNT
        }
      case (count, ((weakRef1, ref2), eq)) => weakRef1.get match {
        case Some(ref1) if (eq.fun(ref1, ref2)) => count + 1 // Inherit unstable count from previous head
        case _                                  => UNSTABLE_COUNT
      }
    }
    createAndStoreVariant(id, refs, refCounts.toArray, recompile, guardFuns)
  }

  /**
   * Computes the next unstable counts of the chosen existing variant. If the
   * variant was just promoted to the head, the previous head needs to be
   * provided, and if the values match the old count is inherited. Otherwise,
   * counts are either incremented if the value matches the current one, or the
   * new value is stored with a reset count.
   */
  @inline
  final private def updateVariant(variant: ProgramVariant,
                                  zipped: Seq[(((WeakReference[AnyRef], Any), GuardFunction), Int)],
                                  previousHead: Option[ProgramVariant]) = {
    zipped.zipWithIndex.foreach({
      case ((((oldWeakRef, newRef), guard), count), i) if !isGuarded(count) =>

        val inheritedCount = if (!previousHead.isEmpty) {
          previousHead.get.refCounts(i) match {
            case REQUIRED_COUNT | STABLE_COUNT => None
            case prevCount => previousHead.get.refs(i).get match {
              case Some(prevRef) if guard.fun(prevRef, newRef) => Some(prevCount + 1)
              case _ => None
            }
          }
        } else None

        variant.refCounts(i) = inheritedCount.getOrElse({
          oldWeakRef.get match {
            case Some(oldRef) if guard.fun(oldRef, newRef) => count + 1
            case _ => {
              variant.refs(i) = new WeakReference[AnyRef](newRef.asInstanceOf[AnyRef])
              UNSTABLE_COUNT
            }
          }
        })
      case _ => // Not unstable
    })
  }

  @inline
  final def check[Ret](id: Long, refs: Seq[Any], recompile: Set[Int] => Any): Ret = {
    val guardFuns = guardFunctions.get(id)
    (programVariants.get(id) match {
      case null => compileInitialVariant(id, refs, recompile, guardFuns)
      case variants => checkHead(id, refs, recompile, guardFuns, variants(0)) match {
        case Some(variant) => variant
        case None => checkAll(id, refs, recompile, guardFuns, variants) match {
          case Some(variant) => variant
          case None => programVariants.get(id) match {
            case Nil => // All have been GC'ed, start from zero
              compileInitialVariant(id, refs, recompile, guardFuns)
            case x :: _ => recompileVariant(id, refs, recompile, guardFuns, x)
          }
        }
      }
    }).function.asInstanceOf[Ret]
  }

  @inline
  final private def checkHead(id: Long, refs: Seq[Any], recompile: Set[Int] => Any,
                              guardFuns: Seq[GuardFunction], headVariant: ProgramVariant): Option[ProgramVariant] = {
    // Check whether all guarded values are OK
    val zipped = headVariant.refs.toSeq.zip(refs).zip(guardFuns).zip(headVariant.refCounts)
    val hasKO = zipped.exists({
      case (((oldWeakRef, newRef), guard), count) if isGuarded(count) => oldWeakRef.get match {
        case Some(oldRef) => !guard.fun(oldRef, newRef) // KO if stable or required not equivalent
        case None         => true // guarded value GC'ed => KO
      }
      case _ => false // unstable aren't checked => OK
    })
    if (hasKO) {
      None
    } else {
      updateVariant(headVariant, zipped, None)
      Some(headVariant)
    }
  }

  @inline
  final private def checkAll(id: Long, refs: Seq[Any], recompile: Set[Int] => Any,
                             guardFuns: Seq[GuardFunction], variants: Seq[ProgramVariant]): Option[ProgramVariant] = {

    val GCed = new Array[Boolean](variants.length)

    // Revive weakRefs and check guarded. If guarded has been GCed discard this program variant.
    def reqAndStableGuarded(arg: Seq[((WeakReference[AnyRef], Int), (Any, GuardFunction))], index: Int): Boolean = {
      -1 == arg.indexWhere {
        case ((weakRef, count), (ref2, equivalent)) if isGuarded(count) => weakRef.get match {
          case None       => GCed(index) = true; true
          case Some(ref1) => !equivalent.fun(ref1, ref2)
        }
        case _ => false
      }
    }

    val possibleVariantsWithStableCount: Seq[(ProgramVariant, Int)] = variants.zipWithIndex.collect({
      case (variant, index) if reqAndStableGuarded(variant.refs.toSeq.zip(variant.refCounts).zip(refs.zip(guardFuns)), index) =>
        (variant, variant.refCounts.count(_ == STABLE_COUNT))
    })

    // remove program variants for which some guarded values have been GC'ed
    val nonGCedVariants: Option[Seq[ProgramVariant]] =
      if (GCed.contains(true)) {
        Some(variants zip GCed filter (!_._2) map (_._1))
      } else None

    // Use the variant with the most stable variables or need to recompile.
    if (possibleVariantsWithStableCount.isEmpty) {
      // No variant found, update the cache if some variants were GC'ed
      nonGCedVariants.foreach(programVariants.put(id, _))
      None
    } else {
      // Use the best variant (most stable variables) and promote it to the head (LRU eviction)
      val variant = possibleVariantsWithStableCount.maxBy(t => t._2)._1
      val vars = nonGCedVariants.getOrElse(variants)
      val (reshuffledVariants, oldHead) = if (vars.head != variant) {
        (Some(variant +: vars.diff(Seq(variant))), Some(vars.head))
      } else {
        (nonGCedVariants, None)
      }
      updateVariant(variant, variant.refs.toSeq.zip(refs).zip(guardFuns).zip(variant.refCounts), oldHead)
      reshuffledVariants.foreach(programVariants.put(id, _)) // Update the cache
      Some(variant)
    }
  }
}
