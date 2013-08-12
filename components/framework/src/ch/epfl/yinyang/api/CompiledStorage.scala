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

  type GuardFun = (Any, Any) => Boolean
  /**
   * A guard consists of a guard function and an Int holding the holeId if it is
   * an optional variable, and -1 otherwise.
   */
  final case class GuardFunction(fun: GuardFun, optionalHoleId: Int)

  /**
   * A program variant in the cache consists of the compiled function, to be cast
   * to the right type. Additionally, it stores the values it was compiled for,
   * so they can be checked by the guard at the next possible execution. It also
   * remembers which of the variables were designated as unstable and therefore
   * don't need to be checked by the guard, but could become stable eventually.
   */
  final case class ProgramVariant(val function: Any, val refs: Seq[WeakReference[AnyRef]],
                                  val unstable: List[Boolean])

  // Number of variants of each program that are cashed. TODO not static
  final private val CACHE_SIZE_PER_PROGRAM = 5

  // The singleton DSL class instance is stored by the UID of the class
  final private val DSLInstances = new ConcurrentHashMap[Long, Any]

  // For each DSL instance, a maximum of CACHE_SIZE_PER_PROGRAM compiled variants are cached
  final private val programVariants = new ConcurrentHashMap[Long, List[ProgramVariant]]()

  // For each DSL instance, there is a GuardFunction for each compilation variable
  final private val guardFunctions = new ConcurrentHashMap[Long, List[GuardFunction]]()

  @inline
  final def lookup[Ret](id: Long, dsl: => Ret, guardFuns: => List[GuardFun],
                        optional: List[Int]): Ret = {
    val dslInstance: Any = DSLInstances.get(id)

    (if (dslInstance == null) {
      DSLInstances.put(id, dsl)
      guardFunctions.put(id, guardFuns zip optional map (g => new GuardFunction(g._1, g._2)))
      dsl
    } else dslInstance).asInstanceOf[Ret]
  }

  @inline
  private final def fetch(id: Long, refs: Seq[Any], recompile: Set[Int] => Any): (List[ProgramVariant], List[GuardFunction]) = {
    val variants = programVariants.get(id)
    val guardFuns = guardFunctions.get(id)
    (variants match {
      case null => createAndStoreVariant(id, refs, recompile, guardFuns)
      case _    => variants
    }, guardFuns)
  }

  @inline
  final private def createAndStoreVariant(id: Long, refs: Seq[Any], recompile: Set[Int] => Any,
                                          guardFuns: List[GuardFunction], unstable: List[Boolean] = Nil) = {
    runtimeCompileCount += 1
    val unstableSet: Set[Int] = unstable match {
      case Nil => Set()
      case l   => l zip guardFuns collect ({ case (unst, GuardFunction(_, id)) if unst => id }) toSet
    }
    val g = new ProgramVariant(recompile(unstableSet), refs.asInstanceOf[Seq[AnyRef]].map(x => new WeakReference(x)), unstable)
    val gs = programVariants.get(id) match {
      case null => List(g)
      case gs   => g :: (gs.take(CACHE_SIZE_PER_PROGRAM - 1))
    }
    programVariants.put(id, gs)
    gs
  }

  @inline
  final def check[Ret](id: Long, refs: Seq[Any], recompile: Set[Int] => Any): Ret = {
    val (variants, guardFuns) = fetch(id, refs, recompile)
    val GCed = new Array[Boolean](variants.length)

    val cachedVariant: Option[ProgramVariant] = variants.zipWithIndex.find({
      case (variant, index) => {
        val refOptions = variant.refs.map(_.get)
        // Check whether some weakRefs have been GC'ed, in that case ignore the variant.
        if (refOptions.exists(_.isEmpty)) {
          GCed(index) = true
          false
        } else {
          // Otherwise check whether all guarded values are equivalent
          refOptions.map(_.get).zip(refs).zip(guardFuns)
            .find({ case ((ref1, ref2), equ) => !equ.fun(ref1, ref2) }).isEmpty
        }
      }
    }).map(_._1)

    if (GCed.exists(b => b)) { // remove program variants for which some guarded values have been GC'ed
      programVariants.put(id, variants zip GCed filter (!_._2) map (_._1))
    }

    cachedVariant.getOrElse(createAndStoreVariant(id, refs, recompile, guardFuns).head)
      .function.asInstanceOf[Ret]
  }
}
