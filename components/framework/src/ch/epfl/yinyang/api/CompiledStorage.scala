package ch.epfl.yinyang.runtime

import java.util.concurrent.ConcurrentHashMap
import scala.ref.WeakReference

object YYStorage {
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
  final private case class DSLInstance(classCode: Any, optionalInitiallyStable: Boolean, codeCacheSize: Int)

  /** DSL instances are stored and retrieved by their UID. */
  final private val DSLInstances = new ConcurrentHashMap[Long, DSLInstance]

  type GuardFun = (Any, Any) => Boolean
  /**
   * A guard consists of a guard function and an Int holding the holeId if it is
   * an optional variable, and -1 otherwise.
   */
  final private case class GuardFunction(fun: GuardFun, optionalHoleId: Int)

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
  // Promotion currently only happens on recompilation. TODO periodically check unstable?
  private val MIN_COUNT_TO_PROMOTE_TO_STABLE = 500

  /** For each DSL instance, a maximum of dslInstance.codeCacheSize compiled variants are cached. */
  final private val programVariants = new ConcurrentHashMap[Long, Seq[ProgramVariant]]()

  @inline
  final def lookup[Ret](id: Long, dsl: => Ret, guardFuns: => Seq[GuardFun],
                        optional: Seq[Int], optionalInitiallyStable: Boolean, codeCacheSize: Int): Ret = {
    val dslInstance = DSLInstances.get(id)

    (if (dslInstance == null) {
      DSLInstances.put(id, DSLInstance(dsl, optionalInitiallyStable, codeCacheSize))
      guardFunctions.put(id, guardFuns zip optional map (g => new GuardFunction(g._1, g._2)))
      dsl
    } else dslInstance.classCode).asInstanceOf[Ret]
  }

  @inline
  final private def createAndStoreInitialVariant(id: Long, refs: Seq[Any], recompile: Set[Int] => Any,
                                                 guardFuns: Seq[GuardFunction]): ProgramVariant = {
    val refCounts = guardFuns map {
      case GuardFunction(_, -1) => REQUIRED_COUNT
      case _                    => if (DSLInstances.get(id).optionalInitiallyStable) STABLE_COUNT else UNSTABLE_COUNT
    }
    createAndStoreVariant(id, refs, refCounts.toArray, recompile, guardFuns)
  }

  @inline
  final private def createAndStoreVariant(id: Long, refs: Seq[Any], refCounts: Array[Int],
                                          recompile: Set[Int] => Any, guardFuns: Seq[GuardFunction]): ProgramVariant = {
    runtimeCompileCount += 1

    val unstableSet = refCounts zip guardFuns collect {
      case (1, GuardFunction(_, id)) => id
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
  final private def recompileVariant(id: Long, refs: Seq[Any], recompile: Set[Int] => Any,
                                     guardFuns: Seq[GuardFunction], headVariant: ProgramVariant): ProgramVariant = {
    val refCounts = headVariant.refCounts.zip(headVariant.refs.zip(refs).zip(guardFuns)) map {
      case (count, _) if count == REQUIRED_COUNT => REQUIRED_COUNT
      case (count, ((weakRef1, ref2), eq)) if count == STABLE_COUNT => weakRef1.get match {
        case Some(ref1) if (eq.fun(ref1, ref2)) => STABLE_COUNT
        case _                                  => UNSTABLE_COUNT
      }
      case (count, ((weakRef1, ref2), eq)) if count > MIN_COUNT_TO_PROMOTE_TO_STABLE => weakRef1.get match {
        case Some(ref1) if (eq.fun(ref1, ref2)) => STABLE_COUNT
        case _                                  => UNSTABLE_COUNT
      }
      case (count, ((weakRef1, ref2), eq)) => weakRef1.get match {
        case Some(ref1) if (eq.fun(ref1, ref2)) => count + 1 // Inherit unstable count from previous head
        case _                                  => UNSTABLE_COUNT
      }
      case _ => UNSTABLE_COUNT
    }
    createAndStoreVariant(id, refs, refCounts.toArray, recompile, guardFuns)
  }

  @inline /** 
   * Computes the next unstable counts of the chosen variant. If the variant
   * was at the head already, counts are either incremented or reset depending
   * on the guard check. Otherwise the previousHead has to be provided and the
   * variant inherits the counts from it if the values are equivalent, else
   * they are reset.
   */
  final private def incrementUnstableCounts(variant: ProgramVariant,
                                            zipped: Seq[(((WeakReference[AnyRef], Any), GuardFunction), Int)],
                                            previousHead: Option[ProgramVariant]) = {
    zipped.zipWithIndex.foreach({
      case ((((oldWeakRef, newRef), guard), count), i) if !isGuarded(count) =>
        variant.refCounts(i) = oldWeakRef.get match {
          case None                                       => UNSTABLE_COUNT
          case Some(oldRef) if !guard.fun(oldRef, newRef) => UNSTABLE_COUNT // TODO could update unstable value?
          case Some(oldRef) => previousHead match {
            case None => count + 1
            case Some(prevVariant) => prevVariant.refCounts(i) match {
              case REQUIRED_COUNT | STABLE_COUNT => count + 1
              case prevCount => prevVariant.refs(i).get match {
                case None => count + 1
                case Some(prevRef) =>
                  val eq = zipped(i)._1._2
                  if (eq.fun(prevRef, newRef)) {
                    prevCount + 1
                  } else {
                    UNSTABLE_COUNT
                  }
              }
            }
          }
        }
      case _ =>
    })
  }

  @inline
  final def check[Ret](id: Long, refs: Seq[Any], recompile: Set[Int] => Any): Ret = {
    val guardFuns = guardFunctions.get(id)
    (programVariants.get(id) match {
      case null => createAndStoreInitialVariant(id, refs, recompile, guardFuns)
      case variants => checkHead(id, refs, recompile, guardFuns, variants(0)) match {
        case Some(variant) => variant
        case None => checkAll(id, refs, recompile, guardFuns, variants) match {
          case Some(variant) => variant
          case None => programVariants.get(id) match {
            case Nil => // All have been GC'ed, start from zero
              createAndStoreInitialVariant(id, refs, recompile, guardFuns)
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
      incrementUnstableCounts(headVariant, zipped, None)
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
      incrementUnstableCounts(variant, variant.refs.toSeq.zip(refs).zip(guardFuns).zip(variant.refCounts), oldHead)
      reshuffledVariants.foreach(programVariants.put(id, _)) // Update the cache
      Some(variant)
    }
  }
}
