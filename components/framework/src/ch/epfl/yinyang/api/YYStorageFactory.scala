package ch.epfl.yinyang.api

object YYStorageFactory {
  def getYYStorageString(className: String, functionType: String, retType: String, guardString: String,
                         optionalHoleIds: String, optionalInitiallyStable: Boolean, codeCacheSize: Int,
                         minimumCountToStabilize: Int, refSymbols: List[reflect.runtime.universe.Symbol]): String = {

    val optionalCount = if (optionalInitiallyStable) "STABLE_COUNT" else "UNSTABLE_COUNT"
    val capturedUpdate = refSymbols.zipWithIndex.map({
      case (sym, index) => // TODO can we get rid of this asInstanceOf?
        "classInstance.captured$" + sym.name.decoded + " = refs(" + index + ").asInstanceOf[" + sym.typeSignature + "]"
    }) mkString "\n"

    s"""
new ch.epfl.yinyang.runtime.YYCache() {
  import java.util.concurrent.ConcurrentHashMap
  import scala.collection.mutable.{ DoubleLinkedList, HashSet }
  import scala.ref.WeakReference

  type GuardFun = (Any, Any) => Boolean
  private val REQUIRED_ID = -1
  /**
   * Each DSL program is represented by a singleton instance of the transformed
   * DSL class and attributes controlling cache behavior and stability. It also
   * stores the guards for all compilation variables, the holeIds for optional
   * variables resp. REQUIRED_ID for required variables and the variant cache.
   */
  private val classInstance: $className = new $className()
  private val guards: Array[GuardFun] = $guardString
  private val optionalHoleIds: Array[Int] = $optionalHoleIds
  private val variantCache: DoubleLinkedList[ProgramVariant] = new DoubleLinkedList[ProgramVariant]()

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
  final private case class ProgramVariant(val function: $functionType, val refs: Array[Reference])
  @inline final private def isGuarded(count: Int) = count <= 0
  private val REQUIRED_COUNT = 0
  private val STABLE_COUNT = -1
  private val UNSTABLE_COUNT = 1

  @inline
  override def guardedLookup[FunctionT](refs: Array[Any]): FunctionT = {
    def recompile(unstableMixed: scala.collection.immutable.Set[scala.Int]): $functionType = {
      $capturedUpdate
      classInstance.compile[$retType, $functionType](unstableMixed)
    }

    val cache = variantCache.synchronized {
      variantCache.toList
    }
    (cache match {
      case Nil => compileInitialVariant(refs, recompile)
      case head :: _ => checkHead(refs, recompile, head) match {
        case Right(variant) => variant
        case Left(check)    => checkAll(refs, recompile, cache, check)
      }
    }).function.asInstanceOf[FunctionT]
  }

  private val GUARDS_OK = 1
  private val GUARDS_FAILED = 0
  private val GCED = -1
  @inline
  final private def reqAndStableGuardsOk(variantRefs: Array[Reference], refs: Array[Any]): Int = {
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
  final private def checkHead(refs: Array[Any], recompile: Set[Int] => $functionType,
                              headVariant: ProgramVariant): Either[Int, ProgramVariant] = {
    // Check whether all guarded values are OK
    val check = reqAndStableGuardsOk(headVariant.refs, refs)
    if (check == GUARDS_OK) {
      updateVariant(headVariant, refs, None)
      Right(headVariant)
    } else {
      Left(check)
    }
  }

  @inline
  final private def checkAll(refs: Array[Any], recompile: Set[Int] => $functionType,
                             variants: Seq[ProgramVariant], headCheck: Int): ProgramVariant = {
    val GCedBuilder = HashSet.newBuilder[ProgramVariant]
    var maxVariant: Option[(ProgramVariant, Int)] = None

    if (headCheck == GCED)
      GCedBuilder += variants.head

    variants.tail.foreach({ variant =>
      val res = reqAndStableGuardsOk(variant.refs, refs)
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
        (recompileVariant(refs, recompile, variants.head), false)
      case Some((variant, _)) =>

        (updateVariant(variant, refs, Some(variants.head)), true)
    }
    val variantsToRemove = GCedBuilder.result

    // Update cache
    if (!variantsToRemove.isEmpty || needsInsert) {
      variantCache.synchronized {
        if (needsInsert) {
          variantCache.insert(DoubleLinkedList(variantCache.head))
          variantCache.elem = variant
          variantsToRemove += variant // Variant inserted at head, remove from tail.
        }
        var c = variantCache.next // Ignore head since that's the chosen variant
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

  /**
   * Compiles a new variant using the stability information from the refCounts
   * and stores it using an LRU cache eviction policy.
   */
  @inline
  final private def createAndStoreVariant(refs: Array[Any], refCounts: Array[Int],
                                          recompile: Set[Int] => $functionType): ProgramVariant = {
    ch.epfl.yinyang.runtime.YYStorage.incrementRuntimeCompileCount()

    val unstableSetBuilder = scala.collection.immutable.HashSet.newBuilder[Int]
    assert(refs.size == refCounts.size && refs.size == optionalHoleIds.size) // sanity check
    for (i <- 0 until refCounts.size) {
      if (!isGuarded(refCounts(i))) {
        unstableSetBuilder += optionalHoleIds(i)
      }
    }

    val refArray = new Array[Reference](refs.size)
    for (i <- 0 until refs.size) {
      refArray(i) = Reference(new WeakReference[AnyRef](refs(i).asInstanceOf[AnyRef]), refCounts(i))
    }

    val variant = new ProgramVariant(recompile(unstableSetBuilder.result), refArray)

    variantCache.synchronized {
      val s = variantCache.size
      if (variantCache.isEmpty) {
        variantCache.next = DoubleLinkedList()
        variantCache.next.prev = variantCache
      } else {
        variantCache.insert(DoubleLinkedList(variantCache.elem))
      }
      variantCache.elem = variant

      var i = 2
      var cache = variantCache.next
      while (!cache.isEmpty) {
        if (i > $codeCacheSize) {
          cache.remove // remove current node from linked list
        }
        i += 1
        cache = cache.next // current node still points into list
      }
    }
    variant
  }

  @inline
  final private def compileInitialVariant(refs: Array[Any], recompile: Set[Int] => $functionType): ProgramVariant = {

    val refCounts = new Array[Int](optionalHoleIds.size)
    for (i <- 0 until optionalHoleIds.size) {
      refCounts(i) = if (optionalHoleIds(i) == REQUIRED_ID) REQUIRED_COUNT else $optionalCount
    }

    createAndStoreVariant(refs, refCounts, recompile)
  }

  @inline
  final private def recompileVariant(refs: Array[Any], recompile: Set[Int] => $functionType,
                                     headVariant: ProgramVariant): ProgramVariant = {
    val headRefs = headVariant.refs

    val refCounts = new Array[Int](headRefs.size)
    for (i <- 0 until headRefs.size) {
      val refI = headRefs(i) // Get a particular Reference object
      refCounts(i) = if (refI.count == REQUIRED_COUNT) {
        REQUIRED_COUNT
      } else {
        refI.ref.get match {
          case Some(ref1) if guards(i)(ref1, refs(i)) =>
            if (refI.count == STABLE_COUNT || (refI.count >= $minimumCountToStabilize))
              STABLE_COUNT
            else
              refI.count + 1 // Always use the newest count
          case _ => UNSTABLE_COUNT
        }
      }
    }

    createAndStoreVariant(refs, refCounts, recompile)
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
  final private def updateVariant(variant: ProgramVariant, refs: Array[Any],
                                  previousHead: Option[ProgramVariant]): ProgramVariant = {
    val variantRefs = variant.refs
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
}
"""
  }
}