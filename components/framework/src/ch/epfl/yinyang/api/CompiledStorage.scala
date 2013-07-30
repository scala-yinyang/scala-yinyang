package ch.epfl.yinyang.runtime

import java.util.concurrent.ConcurrentHashMap
import scala.ref.WeakReference

final class GuardState(val refs: Seq[WeakReference[AnyRef]], val function: Any)

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

  // DSL instances
  final private val programs = new ConcurrentHashMap[Long, Any]

  // Guards
  type GuardFunctions = List[(Any, Any) => Boolean]
  
  // Number of variants of each program that are cashed. TODO not static
  final private val CACHE_SIZE_PER_PROGRAM = 5
  final private val guardStates = new ConcurrentHashMap[Long, List[GuardState]]()
  final private val guardFunctions = new ConcurrentHashMap[Long, GuardFunctions]()

  @inline
  final def lookup[Ret](id: Long, dsl: => Ret, guardFuns: => GuardFunctions): Ret = {
    var program: Any = programs.get(id)
    if (program == null) {
      program = dsl
      programs.put(id, program)
      guardFunctions.put(id, guardFuns)
    }

    program.asInstanceOf[Ret]
  }

  @inline
  private final def fetchGuard(id: Long, refs: Seq[Any], recompile: () => Any): (Seq[GuardState], GuardFunctions) = {
    val guards = guardStates.get(id)
    (guards match {
      case null => createAndStoreGuard(id, refs, recompile)
      case _    => guards
    }, guardFunctions.get(id))
  }

  @inline
  final private def createAndStoreGuard(id: Long, refs: Seq[Any], recompile: () => Any) = {
    runtimeCompileCount += 1
    val g = new GuardState(refs.asInstanceOf[Seq[AnyRef]].map(x => new WeakReference(x)), recompile())
    val gs = guardStates.get(id) match {
      case null => List(g)
      case gs   => g :: (gs.take(CACHE_SIZE_PER_PROGRAM - 1))
    }
    guardStates.put(id, gs)
    gs
  }

  @inline
  final def check[Ret](id: Long, refs: Seq[Any], recompile: () => Any): Ret = {
    val (guards, funs) = fetchGuard(id, refs, recompile)

    (guards.find(!_.refs.map(_.apply()).zip(refs).zip(funs).exists(x => !(x._2(x._1._1, x._1._2)))) match {
      case None        => createAndStoreGuard(id, refs, recompile).head
      case Some(guard) => guard
    }).function.asInstanceOf[Ret]
  }
}
