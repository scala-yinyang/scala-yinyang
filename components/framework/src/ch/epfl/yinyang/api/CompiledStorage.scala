package ch.epfl.yinyang.runtime

import java.util.concurrent.ConcurrentHashMap
import scala.ref.WeakReference

final class GuardState(val values: Seq[Any], val refs: Seq[WeakReference[AnyRef]], val function: Any)

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

  @inline
  final def lookup[Ret](id: Long, dsl: => Ret): Ret = {
    var program: Any = programs.get(id)
    if (program == null) {
      program = dsl
      programs.put(id, program)
    }

    program.asInstanceOf[Ret]
  }

  // Guards
  final private val guards = new ConcurrentHashMap[Long, GuardState]()

  @inline
  private final def fetchGuard(id: Long, values: Seq[Any], refs: Seq[Any], recompile: () => Any): GuardState = {
    val guard = guards.get(id)
    guard match {
      case null => createAndStoreGuard(id, values, refs, recompile)
      case _    => guard
    }
  }

  // avoids instantiation of the arguments
  @inline
  final private def createAndStoreGuard(id: Long, values: Seq[Any], refs: Seq[Any], recompile: () => Any) = {
    runtimeCompileCount += 1
    val g = new GuardState(values, refs.asInstanceOf[Seq[AnyRef]].map(x => new WeakReference(x)), recompile())
    guards.put(id, g)
    g
  }

  @inline
  final def checkRef[Ret](id: Long, values: Seq[Any], refs: Seq[Any], recompile: () => Any): Ret = {
    val guard = fetchGuard(id, values, refs, recompile)

    (if (guard.values != values || guard.refs.map(_.apply()) != refs) {
      createAndStoreGuard(id, values, refs, recompile)
    } else {
      guard
    }).function.asInstanceOf[Ret]
  }

  @inline
  final def checkEq[Ret](id: Long, values: Seq[Any], refs: Seq[Any], recompile: () => Any): Ret = {
    val anyRefs = refs.asInstanceOf[Seq[AnyRef]]
    val guard = fetchGuard(id, values, refs, recompile)

    // TODO optimize
    (if (guard.values != values || (guard.refs.map(_.apply()).zip(anyRefs).exists { x => !(x._1 eq x._2) })) {
      createAndStoreGuard(id, values, refs, recompile)
    } else {
      guard
    }).function.asInstanceOf[Ret]
  }
}
