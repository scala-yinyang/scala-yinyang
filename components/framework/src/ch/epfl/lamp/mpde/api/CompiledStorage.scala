package ch.epfl.lamp.yinyang.runtime

import java.util.concurrent.ConcurrentHashMap
import scala.ref.WeakReference

final class GuardState(val values: Seq[Any], val refs: Seq[WeakReference[AnyRef]], var function: Any)

object YYStorage {

  // DSL instances
  final private val programs = new ConcurrentHashMap[Long, Any]

  // @inline
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

  // @inline
  private final def fetchGuard(id: Long, values: Seq[Any], refs: Seq[Any], recompile: () => Any): GuardState = {
    val anyRefs = refs.asInstanceOf[Seq[AnyRef]]
    var guard = guards.get(id)
    if (guard == null) {
      guard = createGuard(values, anyRefs, recompile)
      guards.put(id, guard)
    }
    guard
  }

  // avoids instantiation of the arguments
  // @inline
  final private def createGuard(values: Seq[Any], refs: Seq[AnyRef], recompile: () => Any) =
    new GuardState(values, refs.map(x => new WeakReference(x)), recompile())

  // @inline
  final def checkRef[Ret](id: Long, values: Seq[Any], refs: Seq[Any], recompile: () => Any): Ret = {
    val guard = fetchGuard(id, values, refs, recompile)

    if (guard.values != values && guard.refs.map(_.apply()) != refs)
      guard.function = recompile()

    guard.function.asInstanceOf[Ret]
  }

  // @inline
  final def checkEq[Ret](id: Long, values: Seq[Any], refs: Seq[Any], recompile: () => Any): Ret = {
    val anyRefs = refs.asInstanceOf[Seq[AnyRef]]
    val guard = fetchGuard(id, values, refs, recompile)

    // TODO optimize
    if (guard.values != values && (guard.refs.map(_.apply()).zip(anyRefs).exists { x => !(x._1 eq x._2) }))
      guard.function = recompile()

    guard.function.asInstanceOf[Ret]
  }

}