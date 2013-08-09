package ch.epfl.yinyang.api

import reflect.runtime.universe._

trait BaseYinYang {
  /**
   * Returns whether and how each hole is needed for compilation.
   *   @see [[VarType]] for details about the different types.
   *   @see [[FullyStaged]] and [[FullyUnstaged]] traits.
   *   @param symbols Maps from hole ids to symbols, useful for generating
   *     debugging output.
   *   @return the type of each hole in the symbols list, in the same order.
   */
  def compilationVars(symbols: List[Symbol]): List[VarType]

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
     * Constructs the DSL internal IR node that will represent a mixed variable
     * that carries both an initial value available at code generation time and
     * a hole that can be used in the generated code. Only DSLs with dynamic or
     * optional compilation variables need to override this method.
     */
    def mixed(v: T, hole: Ret): Ret = ???
  }

  /**
   * Method that replaces captured identifiers of the DSL body.
   */
  def hole[T, Ret](tpe: TypeTag[T], symbolId: Int)(implicit liftEv: LiftEvidence[T, Ret]): Ret =
    liftEv.hole(tpe, symbolId)

  /**
   * Method that replaces constants and captured identifiers required for run-time
   * optimizations in the DSL body.
   */
  def lift[T, Ret](v: T)(implicit liftEv: LiftEvidence[T, Ret]): Ret =
    liftEv.lift(v)

  /**
   * Method that replaces constants and captured identifiers required for run-time
   * optimizations in the DSL body.
   */
  def mixed[T, Ret](v: T, hole: Ret)(implicit liftEv: LiftEvidence[T, Ret]): Ret =
    liftEv.mixed(v, hole)
}
