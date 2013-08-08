package ch.epfl.yinyang.api
import reflect.runtime.universe._

/**
 * A VarType describes the free variables of a DSL program that are used in
 * compilation. Variables which aren't used in any compilation decisions should
 * be marked as `NonCompVar` and will result in holes in the compiled DSL.
 * There are four `CompVar` types, each with an associated guard function that
 * decides when recompilation is triggered.
 *
 * Firstly, a variable can be static or dynamic. If it is static, then values
 * with different executions always need recompilation. But if for example we
 * have multiplication variants for sparse and dense matrices, then we only
 * need to recompile when the new value doesn't fall into the same category.
 * So variables need to be marked as dynamic if some values don't require
 * recompilation but should lead to a different execution anyhow (they appear
 * as variables in the compiled code instead of being fixed to the value used
 * when compiling). In this case, the DSL needs to implement the function:
 * LiftEvidence[T: TypeTag, Ret].lift(v: T, hole: Option[Ret] = None): Ret
 * The value v is used for optimization decisions (e.g. sparse vs. dense
 * matrix), and the hole as variable in the generated code.
 *
 * The second characteristic is whether a variable is required for compilation
 * or optional. For optional variables, runtime statistics are being collected
 * and optimized code is only generated when a variable is sufficiently stable,
 * otherwise generic code with a variable should be generated. The decision
 * which one to use is passed as an Array[Boolean] to the generateCode method,
 * indicating for each optional variable whether it should be treated as a lift
 * or a hole. For required variables, optimized code will always be generated.
 */
abstract class VarType {
  /**
   * Composes two VarTypes, resulting into the VarType with the higher of each
   * characteristic and composed guards:
   * CompVar > NonCompVar
   * Dynamic > Static
   * Required > Optional
   * For example:
   * - OptionalStaticCompVar(g).and(NonCompVar) -> OptionalStaticCompVar(g)
   * - RequiredStaticCompVar(g1).and(OptionalDynamicCompVar(g2))
   *         -> RequiredDynamicCompVar(g1.and(g2))
   */
  def and(other: VarType): VarType
}

case class NonCompVar extends VarType {
  override def and(other: VarType) = other
}

trait CompVar extends VarType {
  def guard: Guard
  def and(guard: Guard): VarType
}

trait Optional
trait Required
trait Static
trait Dynamic

case class OptionalStaticCompVar(val guard: Guard) extends CompVar with Optional with Static {
  def and(guard: Guard) = OptionalStaticCompVar(guard.and(this.guard))
  def and(other: VarType) = other match {
    case NonCompVar() => this
    case o: CompVar   => o.and(guard)
  }
}

case class OptionalDynamicCompVar(val guard: Guard) extends CompVar with Optional with Dynamic {
  def and(guard: Guard) = OptionalDynamicCompVar(guard.and(this.guard))
  def and(other: VarType) = other match {
    case NonCompVar()             => this
    case OptionalStaticCompVar(g) => OptionalDynamicCompVar(guard.and(g))
    case o: CompVar               => o.and(guard)
  }
}

case class RequiredStaticCompVar(val guard: Guard) extends CompVar with Required with Static {
  def and(guard: Guard) = RequiredStaticCompVar(guard.and(this.guard))
  def and(other: VarType) = other match {
    case NonCompVar()              => this
    case OptionalStaticCompVar(g)  => RequiredStaticCompVar(guard.and(g))
    case OptionalDynamicCompVar(g) => RequiredDynamicCompVar(guard.and(g))
    case o: CompVar                => o.and(guard)
  }
}
object DefaultCompVar extends RequiredStaticCompVar(Guard.defaultGuard)

case class RequiredDynamicCompVar(val guard: Guard) extends CompVar with Required with Dynamic {
  def and(guard: Guard) = RequiredDynamicCompVar(guard.and(this.guard))
  def and(other: VarType) = other match {
    case NonCompVar() => this
    case v @ (OptionalStaticCompVar(_) | OptionalDynamicCompVar(_) | RequiredStaticCompVar(_)) =>
      RequiredDynamicCompVar(guard.and(v.asInstanceOf[CompVar].guard))
    case o: CompVar => o.and(guard)
  }
}

/** This object provides a set of operations to create `Guard` values. */
object Guard {
  private val trueTree = "true"
  private val eqTree = "t1.equals(t2)"
  private val eqeqTree = "t1 == t2"
  private val eqeqeqTree = s"$eqTree && $eqeqTree"
  private val eqeq = new Guard(Nil, false, true) {
    override def getGuardBody(): String = eqeqTree
  }

  /** This is the neutral element for guard composition and should not be used otherwise. */
  val always_true: Guard = new Guard(Nil, false, false) {
    override def and(other: Guard): Guard = other
    override def getGuardBody(): String = trueTree
  }

  /** The default guard compares values based on ==. */
  val defaultGuard = eqeq

  /** The equals guard compares values based on .equals. */
  val equals: Guard = new Guard(Nil, true, false) {
    override def getGuardBody(): String = eqTree
  }

  /** The == guard compares values based on ==. */
  def ==(): Guard = eqeq

  /**
   * A custom guard is defined by code that compares values t1 and t2 of type
   * Any and produces a Boolean indicating whether they are equivalent
   * (otherwise the code will be recompiled). As an example, the following
   * String would be used when only integer parity matters for compilation:
   * "t1.asInstanceOf[scala.Int] % 2 == t2.asInstanceOf[scala.Int] % 2"
   */
  def custom(t1Equivalentt2: String) = new Guard(List(t1Equivalentt2), false, false) {
    override def and(other: Guard): Guard = other match {
      case Guard(cust, eq, eqeq) => new Guard(t1Equivalentt2 :: (cust.filter(_ != t1Equivalentt2)), eq, eqeq)
    }
  }
}

/**
 * A guard defines how compilation variables are guarded, e.g. when
 * recompilation is triggered. It encapsulates the body of an anonymous
 * function deciding whether two arguments named "t1" and "t2" of type Any are
 * equivalent as far as the DSL optimizations are concerned, or whether the
 * program has to be recompiled.
 */
case class Guard private (custom: List[String], equals: Boolean, equalsEquals: Boolean) {

  /** Composes two guards. */
  def and(other: Guard): Guard = other match {
    case Guard(cust, eq, eqeq) => new Guard(custom ++ (cust diff custom), equals || eq, equalsEquals || eqeq)
  }

  protected def getGuardBody(): String = {
    import Guard._

    // We optimize the guard function at compile time to get max performance at runtime
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
