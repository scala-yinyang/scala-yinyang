package ch.epfl.yinyang.api

import reflect.runtime.universe._

trait BaseYinYang {
  /**
   * Returns the holes required for run-time optimizations.
   * Compile-time optimized DSLs should return `Nil`.
   *   @param symbols Maps from hole ids to symbols.
   *   @return list of hole symbols indicating which variables are required at
   *     code generation time for optimizations. Each variable has a guard that
   *     defines when recompilation is necessary. The guard also marks whether
   *     the variable is static (can be lifted, e.g. promoted to a constant
   *     after code generation) or dynamic (needs to be a lifted hole, because
   *     not every value change will trigger recompilation).
   */
  def compilationVars(symbols: List[Symbol]): List[(Symbol, Guard)]

  /**
   * Abstract super class for implicit lifters that the DSL author needs to provide.
   */
  abstract class LiftEvidence[T: TypeTag, Ret] {
    /**
     * Constructs the DSL internal IR node that will represent a hole.
     *   @param tpe Represents the run-time type information for this hole.
     *   @param symbolId informs the DSL about the unique identifier of this
     *     hole. This information can be passed back to Yin-Yang by the
     *     `compilationVars` method.
     *   @return DSL internal representation of a hole for type T.
     */
    def hole(tpe: TypeTag[T], symbolId: Int): Ret

    /**
     * Constructs the DSL internal IR node that will represent a constant.
     */
    def lift(v: T): Ret

    /**
     * Constructs the DSL internal IR node needed for dynamic compilationVars.
     * It will represent both a value available at code generation time, and a
     * hole in the generated code. DSLs without dynamic compilationVars don't
     * need to override this method.
     */
    def lift(v: T, hole: Option[Ret] = None): Ret = ???
  }

  /**
   * Method that replaces captured identifiers of the DSL body.
   */
  def hole[T, Ret](tpe: TypeTag[T], symbolId: Int)(implicit liftEv: LiftEvidence[T, Ret]): Ret =
    liftEv hole (tpe, symbolId)

  /**
   * Method that replaces constants and captured identifiers required for run-time
   * optimizations in the DSL body.
   */
  def lift[T, Ret](v: T, hole: Option[Ret] = None)(implicit liftEv: LiftEvidence[T, Ret]): Ret = hole match {
    case None => liftEv lift (v)
    case _    => liftEv lift (v, hole)
  }
}
