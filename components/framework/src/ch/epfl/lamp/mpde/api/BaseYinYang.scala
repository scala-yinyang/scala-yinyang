package ch.epfl.lamp.yinyang.api

import reflect.runtime.universe._

trait BaseYinYang {

  /**
   * Returns a list of holes required for run-time optimizations.
   * Compile-time optimized DSLs should return `Nil`.
   *   @return list of holes required for run-time optimizations. Holes will be promoted
   *           into constants in the next stage of compilation (at runtime).
   */
  def requiredHoles: List[Int]

  /**
   * Abstract super class for implicit lifters that the DSL author needs to provide.
   */
  abstract class LiftEvidence[T: TypeTag, Ret] {
    /**
     * Constructs the DSL internal IR node that will represent a hole.
     *   @param tpe Represents the run-time type information for this hole.
     *   @param symbolId informs the DSL about the unique identifier of this hole.
     *          This information can be passed back to Yin-Yang byt the `requiredHoles` method.
     *   @return DSL internal representation of a hole for type T.
     */
    def hole(tpe: TypeTag[T], symbolId: Int): Ret

    /**
     * Constructs the DSL internal IR node that will represent a constant.
     *
     */
    def lift(v: T): Ret
  }

  /**
   * Method that replaces captured identifiers of the DSL body.
   */
  def hole[T, Ret](tpe: TypeTag[T], symbolId: Int)(implicit liftEv: LiftEvidence[T, Ret]): Ret =
    liftEv hole (tpe, symbolId)

  /**
   * Method that replaces constants and captured identifiers requried for run-time
   * optimizations in the DSL body.
   */
  def lift[T, Ret](v: T)(implicit liftEv: LiftEvidence[T, Ret]): Ret =
    liftEv lift (v)

}