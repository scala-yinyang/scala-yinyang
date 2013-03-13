package ch.epfl.lamp.mpde.api

import scala.collection.mutable.WeakHashMap

object CompiledStorage {

  private val map = WeakHashMap[String, (List[Any], () ⇒ Any)]()

  private def apply(key: String): (List[Any], () ⇒ Any) =
    map(key)

  private def initialized(dsl: String): Boolean =
    !map.get(dsl).isEmpty

  private def init(dsl: String, current: List[Any], program: () ⇒ Any): Unit =
    map.update(dsl, (current, program))

  def checkAndUpdate[T](dsl: String, current: List[Any], recompile: () ⇒ () ⇒ T): () ⇒ T = {
    if (!initialized(dsl)) {
      println("init")
      val program = recompile()
      init(dsl, current, program)
      program
    } else {
      val (previous, compiled) = apply(dsl)
      if (previous.length != current.length || (previous zip current exists { case (old, now) ⇒ old != now })) {
        println("recompile")
        val recompiled = recompile()
        map.update(dsl, (current, recompiled))
        recompiled
      } else {
        println("retrieve")
        compiled.asInstanceOf[() ⇒ T]
      }
    }
  }

}
