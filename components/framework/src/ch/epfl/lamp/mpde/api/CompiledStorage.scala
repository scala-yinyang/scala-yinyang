package ch.epfl.lamp.yinyang
package api

import scala.collection.mutable.WeakHashMap

object CompiledStorage {

  private val map = WeakHashMap[Int, (List[Any], Any)]()

  private def apply(id: Int): (List[Any], Any) =
    map(id)

  private def initialized(id: Int): Boolean =
    !map.get(id).isEmpty

  private def init(id: Int, current: List[Any], program: Any): Unit =
    map.update(id, (current, program))

  def checkAndUpdate[Ret](id: Int, current: List[Any], recompile: () ⇒ Any): Ret =
    if (!initialized(id)) {
      println("init")
      val program = recompile()
      init(id, current, program)
      program.asInstanceOf[Ret]
    } else {
      val (previous, compiled) = apply(id)
      if (previous.length != current.length || (previous zip current exists { case (old, now) ⇒ old != now })) {
        println("recompile")
        val recompiled = recompile()
        map.update(id, (current, recompiled))
        recompiled
      } else {
        println("retrieve")
        compiled
      }
    }.asInstanceOf[Ret]

}
