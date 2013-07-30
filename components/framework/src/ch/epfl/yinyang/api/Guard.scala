package ch.epfl.yinyang.api
import reflect.runtime.universe._

object Guard {
  private val trueTree = "true"
  private val eqTree = "t1.equals(t2)"
  private val eqeqTree = "t1 == t2"
  private val eqeqeqTree = s"$eqTree && $eqeqTree"
  private val eqeq = new Guard(Nil, false, true, false) {
    override def getGuardBody(): String = eqeqTree
  }

  /** This is the neutral element for guard composition and should not be used otherwise. */
  val always_true: Guard = new Guard(Nil, false, false, false) {
    override def and(other: Guard): Guard = other
    override def getGuardBody(): String = trueTree
  }

  /** The default guard compares values based on == and is assumed static. */
  val defaultGuard = eqeq

  /** The equals guard compares values based on .equals and is assumed static. */
  val equals: Guard = new Guard(Nil, true, false, false) {
    override def getGuardBody(): String = eqTree
  }

  /** The == guard compares values based on == and is assumed static. */
  def ==(): Guard = eqeq

  /**
   * A custom guard is defined by code that compares values t1 and t2 of type
   * Any and produces a Boolean indicating whether they are equivalent
   * (otherwise the code will be recompiled). If some values are equivalent
   * with respect to compilation (e.g. compile printing code for even vs. odd)
   * but need to be available as variables in the generated code, they need to
   * be marked as dynamic.
   */
  def custom(t1Equivalentt2: String, dynamic: Boolean) = new Guard(List(t1Equivalentt2), false, false, dynamic) {
    override def and(other: Guard): Guard = other match {
      case Guard(cust, eq, eqeq, dyn) => new Guard(t1Equivalentt2 :: (cust.filter(_ != t1Equivalentt2)), eq, eqeq, dyn || dynamic)
    }
  }
}

/**
 * Encapsulates the body of an anonymous function deciding whether two
 * arguments named "t1" and "t2" of type Any are equivalent as far as the DSL
 * optimizations are concerned, or whether the program has to be recompiled.
 *
 * If in some cases different values don't require recompilation, but should
 * lead to a different execution anyhow, they need to be marked as dynamic so
 * that they can be treated as a lift and a hole. In this case, the
 * LiftEvidence[T: TypeTag, Ret].lift(v: T, hole: Option[Ret] = None): Ret
 * function needs to be overriden, so the DSL can use the value v for
 * optimization decisions, and the hole in the generated code.
 */
case class Guard private (custom: List[String], equals: Boolean, equalsEquals: Boolean, dynamic: Boolean) {

  /** Composes two guards. */
  def and(other: Guard): Guard = other match {
    case Guard(cust, eq, eqeq, dyn) => new Guard(custom ++ (cust diff custom),
      equals || eq, equalsEquals || eqeq, dynamic || dyn)
  }

  protected def getGuardBody(): String = {
    import Guard._

    // We optimize the guard function at compile time to get max performance at runtime
    val initTree = this match {
      case Guard(_, false, false, _) => trueTree
      case Guard(_, true, false, _)  => eqTree
      case Guard(_, false, true, _)  => eqeqTree
      case Guard(_, true, true, _)   => eqeqeqTree
    }
    def reduce(exprs: List[String]): String = {
      exprs.tail.foldRight(exprs.head)((ex: String, tr: String) => s"($ex) && ($tr)")
    }
    this match {
      case Guard(Nil, _, _, _)          => initTree
      case Guard(funs, false, false, _) => reduce(funs)
      case Guard(funs, _, _, _)         => reduce(initTree :: funs)
    }
  }

  def getGuardFunction(): String = s"(t1: Any, t2: Any) => ($getGuardBody)"

  def toDynamic(): Guard = if (dynamic) this else new Guard(custom, equals, equalsEquals, true)
  def isDynamic(): Boolean = dynamic
}