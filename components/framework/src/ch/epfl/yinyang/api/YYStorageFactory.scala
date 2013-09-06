package ch.epfl.yinyang.api

import scala.collection.Iterator

object YYStorageFactory {
  def getYYStorageString(className: String, functionType: String, retType: String, allGuards: List[Guard],
                         optionalHoleIds: String, optionalInitiallyStable: Boolean, codeCacheSize: Int,
                         minimumCountToStabilize: Int, refSymbols: List[reflect.runtime.universe.Symbol]): String = {

    val optionalCount = if (optionalInitiallyStable) "STABLE_COUNT" else "UNSTABLE_COUNT"
    val capturedUpdate = refSymbols.zipWithIndex.map({
      case (sym, index) => // TODO can we get rid of this asInstanceOf?
        "classInstance.captured$" + sym.name.decoded + " = refs(" + index + ").asInstanceOf[" + sym.typeSignature + "]"
    }) mkString "\n    "

    val NON_COMP = 0
    val ONLY_REQ = 1
    val MIXED = 2
    val ONLY_OPT = 3

    val guards: Map[Int, List[(List[(String => String, String)], List[(String => String, String)], Int)]] =
      allGuards.zipWithIndex
        .map(g => (g._1.getReqKeys, g._1.getOptKeys, g._2))
        .groupBy({
          case (Nil, Nil, _) => NON_COMP
          case (_, Nil, _)   => ONLY_REQ
          case (Nil, _, _)   => ONLY_OPT
          case _             => MIXED
        })

    def getTypes(guardType: Int): List[String] = {
      guards.getOrElse(guardType, Nil).flatMap(l => (l._1 ++ l._2).map(_._2))
    }

    def getHashMapsType(typeList: List[String], inner: String): String = typeList.foldRight(inner)({
      (tpe, innerString) => "Map[" + tpe + ", " + innerString + "]"
    })
    def getMixedOptType(typeList: List[String]): String = {
      assert(typeList.length > 0) // otherwise don't call me
      val maps = typeList.foldRight("ProgramVariant")({
        (tpe, innerString) => s"(Map[$tpe, $innerString], Option[$innerString])"
      })
      s"SortedSet[(Int, $maps)]"
    }
    val (variantCacheType, variantCacheInit, variantCachePrefix) =
      (getTypes(ONLY_REQ), getTypes(MIXED) ++ getTypes(ONLY_OPT)) match {
        case (Nil, Nil)      => assert(false)
        case (req, Nil)      => (getHashMapsType(req, "ProgramVariant"), "HashMap()", "")
        case (Nil, o :: Nil) => (s"(Map[$o, ProgramVariant], Option[ProgramVariant])", "(HashMap(), None)", "")
        case (Nil, opt) =>
          val optTpe = getMixedOptType(opt)
          (optTpe, "emptySortedSet", s"val emptySortedSet = $optTpe()(Ordering.by(_._1))\n  ")
        case (req, o :: Nil) =>
          val optTpe = s"(Map[$o, ProgramVariant], Option[ProgramVariant])"
          (getHashMapsType(req, optTpe), "HashMap()", "")
        case (req, opt) =>
          val optTpe = getMixedOptType(opt)
          (getHashMapsType(req, optTpe), "HashMap()", s"val emptySortedSet = $optTpe()(Ordering.by(_._1))\n  ")
      }
    val variantCacheDecl = s"${variantCachePrefix}private var variantCache: $variantCacheType = $variantCacheInit"

    def getGuards(guardType: Int): List[(String => String, String, Int)] =
      guards.getOrElse(guardType, Nil).flatMap(l => (l._1 ++ l._2).map(ss => (ss._1, ss._2, l._3)))

    def guardToString(g: (String => String, String, Int)): String = g match {
      case (gfun, tpe, i) => s"{ val v = refs($i).asInstanceOf[" + refSymbols(i).typeSignature + "]; " + gfun("v") + s" }: $tpe"
    }

    // TODO handle opt properly, this should be just getGuards(ONLY_REQ)
    val lookupReqString = getGuards(ONLY_REQ).grouped(10).toList.zipWithIndex match {
      case Nil => "val lookupReq = variantCache"
      case list =>
        val reqLookup = list.map({
          case (Nil, _) => assert(false)
          case (guards, lookupI) => s"val lookup${lookupI + 1} = lookup${lookupI}" +
            guards.map(guardToString(_)).mkString("\n      .flatMap(_.get(", "))\n      .flatMap(_.get(", "))") +
            s"\n    if (lookup${lookupI + 1}.isEmpty) return None"
        }).mkString("\n    ")
        s"""val lookup0 = Some(variantCache)
    $reqLookup
    val lookupReq = lookup${list.length}.get
    """
    }

    val lookupOptString = (getGuards(MIXED) ++ getGuards(ONLY_OPT)).zipWithIndex match {
      case Nil               => "Some(lookupReq)"
      case (guard, _) :: Nil => s"lookupReq._1.get(${guardToString(guard)}).orElse(lookupReq._2)"
      case guards =>
        val optLookup = guards.init.foldRight(
          s"""val p = map.get(${guardToString(guards.last._1)}).orElse(option); if (!p.isEmpty) return p
      """)({
            case ((guard, i), inner) =>
              val indent = " " * i * 2
              s"""List(map.get(${guardToString(guard)}), option).collect({ case Some((map, option)) => 
        ${indent}$inner})"""
          })
        s"""lookupReq.foreach({ case (_, (map, option)) => 
      $optLookup 
    })
    None
    """
    }

    val storeString = (getGuards(ONLY_REQ) ++ getGuards(MIXED) ++ getGuards(ONLY_OPT)).zipWithIndex match {
      case Nil               => ???
      case (guard, _) :: Nil => s"""variantCache += ((${guardToString(guard)}, variant))"""
      case list => """val nextMap0 = variantCache
    variantCache = """ + list.init.foldRight(list.last match {
        case (guard, hashI) => s"""nextMap${hashI} + ((${guardToString(guard)}, variant))
    """
      })({
        case ((guard, hashI), inner) =>
          val indent = " " * hashI * 2
          s"""{
      ${indent}val H${hashI} = ${guardToString(guard)}
      ${indent}nextMap${hashI}.get(H${hashI}) match {
       ${indent}case None => nextMap${hashI} + ((H${hashI}, ${
            list.drop(hashI + 1).foldRight("variant")({
              case ((guard, hashI), inner) => s"HashMap((${guardToString(guard)}, $inner))"
            })
          }))
       ${indent}case Some(nextMap${hashI + 1}) => nextMap${hashI} + ((H${hashI}, $inner))}}"""
      })
    }

    val theCache = s"""
new ch.epfl.yinyang.runtime.YYCache() {
  import scala.collection.immutable.{ HashMap, SortedSet }

  final private case class ProgramVariant(val function: $functionType)

  private val classInstance: $className = new $className()
  
  @inline
  private def recompile(refs: Array[Any], unstableSet: scala.collection.immutable.Set[scala.Int]): $functionType = {
    ch.epfl.yinyang.runtime.YYStorage.incrementRuntimeCompileCount()
    $capturedUpdate
    classInstance.compile[$retType, $functionType](unstableSet)
  }

  $variantCacheDecl

  @inline
  private def lookup(refs: Array[Any]): Option[ProgramVariant] = {
    $lookupReqString
    $lookupOptString
  }

// TODO unstableSet, store
  @inline
  private def createAndStoreVariant(refs: Array[Any]): ProgramVariant = {
    val variant = ProgramVariant(recompile(refs, Set()))
    $storeString
    variant
  }

  @inline
  override def guardedLookup[FunctionT](refs: Array[Any]): FunctionT = {
    lookup(refs).getOrElse(createAndStoreVariant(refs)).function.asInstanceOf[FunctionT]
  }


}"""

    println(s"""=========
$theCache
=======""")
    theCache
  }
}

/*
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
  private val guards: Array[GuardFun] = null
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
    def recompile(unstableSet: scala.collection.immutable.Set[scala.Int]): $functionType = {
      $capturedUpdate
      classInstance.compile[$retType, $functionType](unstableSet)
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
  */
