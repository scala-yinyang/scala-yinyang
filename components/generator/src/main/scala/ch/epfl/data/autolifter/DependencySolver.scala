package ch.epfl.data
package autolifter

import annotations.{ Custom, CMethod }
import scala.collection.mutable.StringBuilder
import scala.collection.mutable.ArrayBuffer
import scala.reflect.api.Universe
import scala.reflect.runtime.universe._
import types._

// Dependency solver (experimental)
object DependencySolver {
  private val dependantTypes = ArrayBuffer[Universe#Type]()
  private val alreadyLiftedTypes = ArrayBuffer[Universe#Type]()
  private val sb = new StringBuilder(4096, "")

  def registerDependantType(t: Universe#Type) {
    if ((t.toString.startsWith("scala.collection") || t.typeSymbol.toString.startsWith("class Tuple")) &&
      (!isTypeRegistered(t) && !isTypeLifted(t)))
      dependantTypes += t
  }

  def markTypeLifted(t: Universe#Type) {
    dependantTypes.find(e => e.typeSymbol.fullName == t.typeSymbol.fullName) match {
      case Some(p) => dependantTypes -= p
      case None    =>
    }
    if (!isTypeLifted(t))
      alreadyLiftedTypes += t
  }

  def isTypeLifted(t: Universe#Type) = alreadyLiftedTypes.exists(e => e.typeSymbol.fullName == t.typeSymbol.fullName)

  def isTypeRegistered(t: Universe#Type) = dependantTypes.exists(e => e.typeSymbol.fullName == t.typeSymbol.fullName)

  def liftAllDependencies(config: Custom, al: AutoLifter): String = {
    dependantTypes.clone.foreach(t => {
      if (!isTypeLifted(t)) {
        println("Resolving " + t)
        val x = al.autoLiftType(config)(t)
        sb ++= x
        markTypeLifted(t)
        println("DONE!")
      }
    })
    if (dependantTypes.size != 0) liftAllDependencies(config, al)
    sb.toString
  }

  // Debug
  def printDependantTypes() { println(dependantTypes.mkString("\n")) }
}
