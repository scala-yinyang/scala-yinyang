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
 * LiftEvidence[T: TypeTag, Ret].mixed(v: T, hole: Ret): Ret
 * The value v is used for optimization decisions (e.g. sparse vs. dense
 * matrix), and the hole as variable in the generated code.
 *
 * The second characteristic is whether a variable is required for compilation
 * or optional. For required variables, optimized code will always be
 * generated. For optional variables, runtime statistics are being collected
 * and optimized code is only generated when a variable is sufficiently stable,
 * otherwise generic code with a variable should be generated. Optional
 * variables are also represented as mixed nodes in the DSL body. The stable
 * variables are passed as a set of holeIds to the generateCode method and can
 * be treated like required variables, respecting their static/dynamic nature,
 * and will be guarded with the provided guard function. For the unstable ones,
 * generic code only using the hole and not the value of the mixed node has to
 * be generated. The value will not be guarded.
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
object CompVar {
  def equality(tpe: String) = RequiredStaticCompVar(List(({ v: String => v }, tpe)))
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
object OptionalStaticCompVar {
  def apply(optKeys: List[(String => String, String)]): OptionalStaticCompVar = OptionalStaticCompVar(Guard(Nil, optKeys))
  def equality(tpe: String) = OptionalStaticCompVar(List(({ v: String => v }, tpe)))
}

case class OptionalDynamicCompVar(val guard: Guard) extends CompVar with Optional with Dynamic {
  def and(guard: Guard) = OptionalDynamicCompVar(guard.and(this.guard))
  def and(other: VarType) = other match {
    case NonCompVar()             => this
    case OptionalStaticCompVar(g) => OptionalDynamicCompVar(guard.and(g))
    case o: CompVar               => o.and(guard)
  }
}
object OptionalDynamicCompVar {
  def apply(optKeys: List[(String => String, String)]): OptionalDynamicCompVar = OptionalDynamicCompVar(Guard(Nil, optKeys))
  def equality(tpe: String) = OptionalDynamicCompVar(List(({ v: String => v }, tpe)))
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
object RequiredStaticCompVar {
  def apply(reqKeys: List[(String => String, String)]): RequiredStaticCompVar = RequiredStaticCompVar(Guard(reqKeys, Nil))
  def equality(tpe: String) = RequiredStaticCompVar(List(({ v: String => v }, tpe)))
}

case class RequiredDynamicCompVar(val guard: Guard) extends CompVar with Required with Dynamic {
  def and(guard: Guard) = RequiredDynamicCompVar(guard.and(this.guard))
  def and(other: VarType) = other match {
    case NonCompVar() => this
    case v @ (OptionalStaticCompVar(_) | OptionalDynamicCompVar(_) | RequiredStaticCompVar(_)) =>
      RequiredDynamicCompVar(guard.and(v.asInstanceOf[CompVar].guard))
    case o: CompVar => o.and(guard)
  }
}
object RequiredDynamicCompVar {
  def apply(reqKeys: List[(String => String, String)]): RequiredDynamicCompVar = RequiredDynamicCompVar(Guard(reqKeys, Nil))
  def equality(tpe: String) = RequiredDynamicCompVar(List(({ v: String => v }, tpe)))
}

/**
 * A guard defines how compilation variables are guarded, e.g. when
 * recompilation is triggered. It encapsulates the body of an anonymous
 * function deciding whether two arguments named "t1" and "t2" of type Any are
 * equivalent as far as the DSL optimizations are concerned, or whether the
 * program has to be recompiled.
 */
case class Guard(private val reqKeys: List[(String => String, String)],
                 private val optKeys: List[(String => String, String)]) {
  /** Composes two guards. */
  def and(other: Guard): Guard = other match {
    case Guard(rk, ok) => new Guard(reqKeys ++ rk, optKeys ++ ok)
  }
  override def toString = {
    val req = getReqKeys.map({ case (fun, tpe) => s"{ v => (" + fun("v") + s"): $tpe }" }).mkString("(", ", ", ")")
    val opt = getOptKeys.map({ case (fun, tpe) => s"{ v => (" + fun("v") + s"): $tpe }" }).mkString("(", ", ", ")")
    s"Guard(req: $req, opt: $opt)"
  }

  lazy val getReqKeys = reqKeys.distinct
  lazy val getOptKeys = optKeys.distinct diff reqKeys
}
