package ch.epfl.yinyang.api
import reflect.runtime.universe._

object Guard {
  private val trueTree = "true"
  private val eqTree = "t1.equals(t2)"
  private val eqeqTree = "t1 == t2"
  private val eqeqeqTree = s"$eqTree && $eqeqTree"
  private val eqeq = new Guard(Nil, false, true) {
    override def getGuardBody(): String = eqeqTree
  }

  val defaultGuard = eqeq

  val always_true: Guard = new Guard(Nil, false, false) {
    override def and(other: Guard): Guard = other
    override def getGuardBody(): String = trueTree
  }
  val equals: Guard = new Guard(Nil, true, false) {
    override def getGuardBody(): String = eqTree
  }
  def ==(): Guard = eqeq
  def custom(t1Equivalentt2: String) = new Guard(List(t1Equivalentt2), false, false) {
    override def and(other: Guard): Guard = other match {
      case Guard(cust, eq, eqeq) => new Guard(t1Equivalentt2 :: (cust.filter(_ != t1Equivalentt2)), eq, eqeq)
    }
  }
}

/**
 * Encapsulates the body of an anonymous function deciding whether two
 * arguments named t1 and t2 of type Any are equivalent as far as the DSL
 * optimizations are concerned, or whether the program has to be recompiled.
 */
case class Guard private (custom: List[String], equals: Boolean, equalsEquals: Boolean) {

  def and(other: Guard): Guard = other match {
    case Guard(cust, eq, eqeq) => new Guard(custom ++ (cust diff custom), equals || eq, equalsEquals || eqeq)
  }

  // We optimize the guard function at compile time to get max performance at runtime
  import Guard._
  protected def getGuardBody(): String = {

    val initTree = this match {
      case Guard(_, false, false) => trueTree
      case Guard(_, true, false)  => eqTree
      case Guard(_, false, true)  => eqeqTree
      case Guard(_, true, true)   => eqeqeqTree
    }
    def reduce(exprs: List[String]): String = {
      exprs.tail.foldRight(exprs.head)((ex: String, tr: String) => s"($ex) && ($tr)")
    }
    this match {
      case Guard(Nil, _, _)          => initTree
      case Guard(funs, false, false) => reduce(funs)
      case Guard(funs, _, _)         => reduce(initTree :: funs)
    }
  }

  def getGuardFunction(): String = s"(t1: Any, t2: Any) => ($getGuardBody)"
}