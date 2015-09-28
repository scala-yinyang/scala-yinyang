package ch.epfl.yinyang.api
import scala.collection.Iterator

object YYStorageFactory {
  def getYYStorageString(className: String, functionType: String, retType: String, allGuards: List[Guard],
                         optionalHoleIds: String, optionalInitiallyStable: Boolean, codeCacheSize: Int,
                         minimumCountToStabilize: Int, refSymbols: List[reflect.runtime.universe.Symbol]): String = {
    val indent = "      "

    val capturedUpdate = refSymbols.zipWithIndex.map({
      case (sym, index) => // TODO can we get rid of this asInstanceOf?
        "classInstance.captured$" + sym.name.decodedName.toString + " = refs(" + index + ").asInstanceOf[" + sym.typeSignature + "]"
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

    def getGuards(guardType: Int): List[(String => String, String, Int)] =
      guards.getOrElse(guardType, Nil).flatMap(l => (l._1 ++ l._2).map(ss => (ss._1, ss._2, l._3)))
    val onlyReq = getGuards(ONLY_REQ)
    val mixed = getGuards(MIXED)
    val onlyOpt = getGuards(ONLY_OPT)
    val allOpt = mixed ++ onlyOpt
    val hasOpt = allOpt.length > 0

    def getTypes(guardType: Int): List[String] = {
      guards.getOrElse(guardType, Nil).flatMap(l => (l._1 ++ l._2).map(_._2))
    }

    def getHashMapsType(typeList: List[String], inner: String): String = typeList.foldRight(inner)({
      (tpe, innerString) => "Map[" + tpe + ", " + innerString + "]"
    })
    def getMixedOptType(typeList: List[String]): String = {
      s"SortedSet[(Int, ${getInnerMixedOptType(typeList)})]"
    }
    def getInnerMixedOptType(typeList: List[String]): String = {
      typeList.foldRight("ProgramVariant")({
        (tpe, innerString) => s"(Map[$tpe, $innerString], Option[$innerString])"
      })
    }
    def getCacheType(level: Int, withSet: Boolean = false) = (getTypes(ONLY_REQ).drop(level), (getTypes(MIXED) ++ getTypes(ONLY_OPT))) match {
      case (Nil, Nil)                                         => "ProgramVariant"
      case (Nil, o :: Nil) if (level == onlyReq.length)       => s"(Map[$o, ProgramVariant], Option[ProgramVariant])"
      case (Nil, opt) if (level == onlyReq.length && withSet) => getMixedOptType(opt)
      case (Nil, opt)                                         => getInnerMixedOptType(opt.drop(level - onlyReq.length))
      case (req, Nil)                                         => getHashMapsType(req, "ProgramVariant")
      case (req, o :: Nil)                                    => getHashMapsType(req, s"(Map[$o, ProgramVariant], Option[ProgramVariant])")
      case (req, opt)                                         => getHashMapsType(req, getMixedOptType(opt))
    }
    val variantCacheType = getCacheType(0, true)
    val (variantCacheInit, variantCachePrefix) = (getTypes(ONLY_REQ), getTypes(MIXED) ++ getTypes(ONLY_OPT)) match {
      case (Nil, Nil)      => ("???", "") //assert(false)
      case (req, Nil)      => ("HashMap()", "")
      case (Nil, o :: Nil) => ("(HashMap(), None)", "")
      case (Nil, opt)      => ("emptySortedSet", s"private val emptySortedSet = $variantCacheType()(Ordering.by(_._1))\n  ")
      case (req, o :: Nil) => ("HashMap()", "")
      case (req, opt)      => ("HashMap()", s"private val emptySortedSet = ${getMixedOptType(opt)}()(Ordering.by(_._1))\n  ")
    }
    val variantCacheDecl = s"${variantCachePrefix}private var variantCache: $variantCacheType = $variantCacheInit"

    val AllOptCounterDecl = """@inline
  final private def isUnstable(count: Int) = count > 0
  private val STABLE_COUNT = -1
  private val UNSTABLE_COUNT = 1
  """ + (getTypes(MIXED) ++ getTypes(ONLY_OPT)).zipWithIndex.map({ case (tpe, i) => (tpe, i + onlyReq.length) })
      .map({ case (tpe, i) => s"var g$i: ($tpe, Int)" })
      .mkString("final private class AllOptCounters(", ", ", ") {}") + """
  private var headVariant: (ProgramVariant, AllOptCounters) = null"""

    val updateCtsString = {
      val newCtr = List.range(onlyReq.length, onlyReq.length + allOpt.length)
        .map(i => s"g$i = (hash$i, if (unstable$i) UNSTABLE_COUNT else STABLE_COUNT)").mkString(", ")
      val incrementCtr = List.range(onlyReq.length, onlyReq.length + allOpt.length)
        .map(i => s"oldCounts.g$i = oldCounts.g$i match { case (hash, count) if (hash == hash$i && isUnstable(count)) => (hash$i, count + 1);" +
          s" case (_, count) if isUnstable(count) => (hash$i, UNSTABLE_COUNT); case stable => stable }")
        .mkString(s"\n${indent}    ")
      val updatedCtr = List.range(onlyReq.length, onlyReq.length + allOpt.length)
        .map(i => s"g$i = (hash$i, if (!unstable$i) STABLE_COUNT else oldCounts.g$i match { " +
          s"case (hash, count) if (hash == hash$i && isUnstable(count)) => count + 1; case _ => UNSTABLE_COUNT } )")
        .mkString("new AllOptCounters(", ", ", ")")

      s"""@inline def updateCts(variant: ProgramVariant): ProgramVariant = {
${indent}variant.unsafeCounter += 1
${indent}headVariant match {
${indent}  case null => headVariant = (variant, new AllOptCounters($newCtr))
${indent}  case (sameVariant, oldCounts) if (variant == sameVariant) =>
${indent}    $incrementCtr
${indent}  case (oldVariant, oldCounts) => headVariant = (variant, $updatedCtr)
${indent}}
${indent}variant
    }"""
    }

    def guardToString(g: (String => String, String, Int)): String = g match {
      case (gfun, tpe, i) => s"{ val v = refs($i).asInstanceOf[" + refSymbols(i).typeSignature + "]; " + gfun("v") + s" }: $tpe"
    }

    val allHashes = onlyReq.map(guardToString(_)).zipWithIndex
      .map({ case (value, i) => s"""val hash$i = $value""" }).mkString("\n    ") + "\n    " +
      allOpt.zipWithIndex.map({ case (g, i) => (guardToString(g), i + onlyReq.length) })
      .map({ case (value, i) => s"""val hash$i = $value; var unstable$i = false""" }).mkString("\n    ")

    val lookupReqString = List.range(0, onlyReq.length).grouped(10).toList.zipWithIndex match {
      case Nil => "val lookupReq = variantCache"
      case list =>
        val reqLookup = list.map({
          case (Nil, _) => assert(false)
          case (hashIs, lookupI) => s"val lookup${lookupI + 1} = lookup${lookupI}\n${indent}" +
            hashIs.map(hashI => s"  .flatMap(_.get(hash${hashI}))").mkString(s"\n${indent}") +
            s"\n${indent}if (lookup${lookupI + 1}.isEmpty) return None"
        }).mkString(s"\n${indent}")
        s"""val lookup0 = Some(variantCache)
${indent}$reqLookup
${indent}val lookupReq = lookup${list.length}.get"""
    }

    val lookupOptString = List.range(onlyReq.length, onlyReq.length + allOpt.length) match {
      case Nil      => "Some(lookupReq)"
      case i :: Nil => s"lookupReq._1.get(hash${i}).orElse({ unstable${i} = true; lookupReq._2 })"
      case is =>
        val ind = indent + (" " * 2 * allOpt.length)
        val optLookup = is.init.foldRight({
          val last = is.last
          s"""map.get(hash$last).map((_, false)).orElse(option.map((_, true))) match {
${ind}  case None =>
${ind}  case Some((variant, unst)) => unstable$last = unst; return Some(variant)
${ind}}"""
        })({
          case (i, inner) =>
            val ind = indent + (" " * (i - onlyReq.length) * 2)
            s"""List(map.get(hash$i), option).collect({ case Some((map, option)) =>
${ind}    $inner
${ind}  })"""
        })
        s"""lookupReq.foreach({ case (_, (map, option)) =>
${indent}  $optLookup
${indent}})
${indent}None"""
    }

    val optRefs = allOpt.map(_._3)

    val unstableSet = (if (!hasOpt) "val unstableSet = Set[Int]()"
    else
      s"val unstableSet = if (headVariant == null) {\n${indent}  " +
        (if (optionalInitiallyStable)
          List.range(onlyReq.length, onlyReq.length + allOpt.length)
          .map("unstable" + _ + " = false").mkString("", s"\n${indent}  ", s"\n${indent}  ") + "Set[Int]()"
        else {
          List.range(onlyReq.length, onlyReq.length + allOpt.length).map("unstable" + _ + " = true")
            .mkString("", s"\n${indent}  ", s"\n${indent}  ") + optRefs.distinct.mkString("Set[Int](", ", ", ")")
        }) + s"""
${indent}} else {
${indent}  val prefCounts = headVariant._2
${indent}  val builder = Set.newBuilder[Int]""" +
        List.range(onlyReq.length, onlyReq.length + allOpt.length).map(i => s"""
${indent}  prefCounts.g$i match {
${indent}    case (hash, count) if (hash == hash$i && (count == STABLE_COUNT || count > $minimumCountToStabilize)) => unstable$i = false
${indent}    case _ => unstable$i = true; builder += ${optRefs(i - onlyReq.length)}
${indent}  }""").mkString + s"""
${indent}  builder.result
${indent}}""")

    def newPath(forGuardIndex: Int, fromEmptySet: Boolean = false): String = {
      val optPath = List.range(Math.max(onlyReq.length, forGuardIndex), onlyReq.length + allOpt.length).foldRight("variant")({
        case (i, inner) => s"{ val inner: ${getCacheType(i + 1)} = $inner; if (unstable${i}) (HashMap(), Some(inner)): ${getCacheType(i)} else (HashMap((hash${i}, inner)), None) }"
      })
      val optPathSet =
        if ((forGuardIndex < onlyReq.length || (fromEmptySet && forGuardIndex == onlyReq.length)) && allOpt.length > 1)
          s"emptySortedSet + ((0, $optPath))"
        else
          optPath

      List.range(forGuardIndex, onlyReq.length).foldRight(optPathSet)({
        case (i, inner) => s"HashMap((hash${i}, $inner))"
      })
    }

    val optCountReset = allOpt.zipWithIndex.map({ case (guard, i) => (guard, i + onlyReq.length) })
      .foldRight({
        val i = onlyReq.length + allOpt.length;
        val ind = indent + (" " * i)
        s"""variantCount += 1
${ind}if (next${i}.unsafeCounter < minCount) {
${ind}  hashes.copyToArray(minHashes); minCount = next${i}.unsafeCounter; next${i}.unsafeCounter = 0; minSet = set
${ind}}"""
      })({
        case ((guard, hashI), inner) =>
          val ind = indent + (" " * hashI)
          s"""(next${hashI}._2 match {
${ind}  case Some(ohm) => (null, ohm) :: next${hashI}._1.toList
${ind}  case None => next${hashI}._1.toList
${ind}}).foreach({ case (hash, next${hashI + 1}) => hashes($hashI) = hash
${ind} $inner })"""
      })
    val optCountResetString =
      if (allOpt.length > 1) s"""next${onlyReq.length}.foreach({ case (i, next${onlyReq.length}) => set = i
${indent + (" " * onlyReq.length)}$optCountReset })"""
      else optCountReset

    val reqCountResetString = onlyReq.zipWithIndex.foldRight(optCountResetString)({
      case ((guard, hashI), inner) =>
        val ind = indent + (" " * hashI)
        s"""next${hashI}.foreach({ case (hash, next${hashI + 1}) => hashes($hashI) = hash;
${ind} $inner })"""
    })

    val countResetString = s"""var minCount = Int.MaxValue
${indent}var variantCount = 0
${indent}val minHashes = new Array[Any](${onlyReq.length + allOpt.length})
${indent}var minSet = 0
${indent}val hashes = new Array[Any](${onlyReq.length + allOpt.length})
${indent}var set = 0
${indent}val next0 = variantCache
${indent}$reqCountResetString"""

    val optEvict = (getTypes(MIXED) ++ getTypes(ONLY_OPT)).zipWithIndex
      .map({ case (tpe, i) => (tpe, i + onlyReq.length) }).foldRight("None: Option[ProgramVariant]")({
        case ((tpe, i), inner) =>
          val ind = indent + "    " + (" " * onlyReq.length) + ("  " * (i - onlyReq.length))
          s"""next$i match {
${ind}case (hm, opt) if (hm.size == 0) => assert(!opt.isEmpty && minHashes($i) == null); None
${ind}case (hm, Some(_)) if (minHashes($i) == null) => if (hm.size == 0) None else Some((hm, None))
${ind}case (hm, opt) => hm.get(minHashes($i).asInstanceOf[$tpe]) match { case None => assert(false); None
${ind} case Some(next${i + 1}) => val inner = $inner
${ind}  if (inner.isEmpty) Some((hm - minHashes($i).asInstanceOf[$tpe], opt)) else Some((hm + ((minHashes($i).asInstanceOf[$tpe], inner.get)), opt)) }}"""
      })

    val optEvictString = {
      val ind = indent + "  " + (" " * onlyReq.length)
      if (allOpt.length > 1) s"""{
${ind}val prev = next${onlyReq.length}.find(_._1 == minSet)
${ind}prev.map({
${ind} case (_, next${onlyReq.length}) => $optEvict
${ind}}) match {
${ind}  case None if (next${onlyReq.length}.size == 1) => None
${ind}  case None => Some(next${onlyReq.length} - prev.get)
${ind}  case Some(innerOHM) => Some(next${onlyReq.length} + ((minSet, innerOHM.get)))
${ind}}}"""
      else optEvict
    }

    val reqEvictString = getTypes(ONLY_REQ).zipWithIndex.foldRight(optEvictString)({
      case ((tpe, i), inner) =>
        val ind = indent + "  " + (" " * i)
        s"""next$i.get(minHashes($i).asInstanceOf[$tpe]) match { case None => assert(false); None
${ind}case Some(next${i + 1}) => val inner = $inner
${ind} inner match { case None => if (next$i.size == 1) None else Some(next$i - (minHashes($i).asInstanceOf[$tpe]))
${ind}  case Some(innerHM) => Some(next$i + ((minHashes($i).asInstanceOf[$tpe], innerHM))) } }"""
    })

    val evictString = s"""val evictedCache = if(variantCount < $codeCacheSize) {
${indent}  variantCache
${indent}} else {
${indent}  val next0 = variantCache
${indent}  ($reqEvictString).get
${indent}}"""

    val optStoreString = List.range(onlyReq.length, onlyReq.length + allOpt.length) match {
      case Nil => "variant"
      case _ :: Nil => s"""if (unstable${onlyReq.length}) (nextMap${onlyReq.length}._1, Some(variant))
${indent + " " + (" " * onlyReq.length)}else (nextMap${onlyReq.length}._1 + ((hash${onlyReq.length}, variant)), nextMap${onlyReq.length}._2)"""
      case list =>
        val optStore = list.foldRight("variant")({
          case (i, inner) =>
            val ind = indent + "   " + (" " * (2 * i - onlyReq.length))
            s"""nextOMap${i} match { case (hm, opt) => if (unstable${i}) opt match { case None => (hm, Some(${newPath(i + 1)}))
${ind}  case Some(nextOMap${i + 1}: ${getCacheType(i + 1)}) => (hm, Some($inner)) }
${ind} else hm.get(hash${i}) match {
${ind}  case None => (hm + ((hash${i}, ${newPath(i + 1)})), opt)
${ind}  case Some(nextOMap${i + 1}: ${getCacheType(i + 1)}) => (hm + ((hash${i}, $inner)), opt) }}"""
        })
        val ind = indent + " " + (" " * onlyReq.length)
        s"""
${ind}nextMap${onlyReq.length}.find(_._1 == nrUnstable) match {
${ind}  case None => nextMap${onlyReq.length} + ((nrUnstable, ${newPath(onlyReq.length)}))
${ind}  case Some((_, nextOMap${onlyReq.length})) => nextMap${onlyReq.length} + ((nrUnstable, $optStore))
${ind}} """
    }

    val storeString = s"""val nextMap0 = evictedCache
${indent}variantCache = """ + (List.range(0, onlyReq.length) match {
      case Nil => optStoreString
      case list =>
        val z = if (allOpt.length == 0) {
          s"nextMap${onlyReq.length - 1} + ((hash${onlyReq.length - 1}, $optStoreString))\n    "
        } else {
          val last = onlyReq.length - 1
          val ind = indent + " " + (" " * last)
          s"""nextMap${last}.get(hash${last}) match {
${ind}case None => nextMap${last} + ((hash${last}, ${newPath(last + 1, true)}))
${ind}case Some(nextMap${last + 1}: ${getCacheType(last + 1, true)}) => nextMap${last} + ((hash${last}, $optStoreString)) }"""
        }
        list.init.foldRight(z)({
          case (hashI, inner) =>
            val ind = indent + " " + (" " * hashI)
            s"""nextMap${hashI}.get(hash${hashI}) match {
${ind}case None => nextMap${hashI} + ((hash${hashI}, ${newPath(hashI + 1, true)}))
${ind}case Some(nextMap${hashI + 1}: ${getCacheType(hashI + 1)}) => nextMap${hashI} + ((hash${hashI}, $inner)) }"""
        })
    })

    val theCache = s"""
new ch.epfl.yinyang.runtime.YYCache() {

  // Guards:
  // onlyReq: ${onlyReq.map(guardToString(_))}
  // mixed: ${mixed.map(guardToString(_))}
  // onlyOpt: ${onlyOpt.map(guardToString(_))}

  import scala.collection.immutable.{ HashMap, SortedSet, Set }

  final private case class ProgramVariant(function: $functionType, var unsafeCounter: Int)

  private val classInstance: $className = new $className()

  @inline
  private def recompile(refs: Array[Any], unstableSet: scala.collection.immutable.Set[scala.Int]): $functionType = {
    ch.epfl.yinyang.runtime.YYStorage.incrementRuntimeCompileCount()
    $capturedUpdate
    classInstance.compile[$retType, $functionType](unstableSet)
  }

  ${if (hasOpt) AllOptCounterDecl else ""}

  $variantCacheDecl

  @inline
  override def guardedLookup[FunctionT](refs: Array[Any]): FunctionT = {

    $allHashes

    @inline
    def lookup(refs: Array[Any]): Option[ProgramVariant] = {

      // Lookup all required variables in their HashMaps
      $lookupReqString

      // Traverse mixed and optional variables ordered by increasing number of
      // unstable variables per variant
      $lookupOptString
    }

    ${if (hasOpt) updateCtsString else ""}

    @inline
    def createAndStoreVariant(refs: Array[Any]): ProgramVariant = {
      $unstableSet
      val nrUnstable = unstableSet.size

      val variant = ProgramVariant(recompile(refs, unstableSet), 0)

      // Find least frequently used variant and reset all counters
      $countResetString

      // If cache capacity is reached, evict the least frequently used variant
      $evictString

      // Store the new variant
      $storeString
      variant
    }

    val variant = lookup(refs).getOrElse(createAndStoreVariant(refs))
    ${if (hasOpt) "updateCts(variant)" else ""}
    variant.function.asInstanceOf[FunctionT]
  }
}"""
    theCache
  }
}
