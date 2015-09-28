package ch.epfl.yinyang.api

import reflect.runtime.universe._

trait BaseYinYang {
  /**
   * Abstraction over different scala runtime types. Once the Manifests are deprecated
   * this abstraction will be removed.
   */
  type TypeRep[T]
  def runtimeType[T: TypeRep]: TypeRep[T]

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
  abstract class LiftEvidence[T: TypeRep, Ret] {
    /**
     * Constructs the DSL internal IR node that will represent a hole.
     *   @param tpe Represents the run-time type information for this hole.
     *   @param symbolId informs the DSL about the unique identifier of this
     *     hole. This information can be passed back to Yin-Yang by the
     *     `compilationVars` method.
     *   @return DSL internal representation of a hole for type T.
     */
    def hole(tpe: TypeRep[T], symbolId: Int): Ret

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
  def hole[T, Ret](tpe: TypeRep[T], symbolId: Int)(implicit liftEv: LiftEvidence[T, Ret]): Ret =
    liftEv hole (tpe, symbolId)

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

/*
 * Component for TypeTag based DSLs.
 */
trait TypeTagBased {
  type TypeRep[T] = TypeTag[T]
  def runtimeType[T: TypeRep]: TypeRep[T] = typeTag[T]
}

/*
 * Component for Manifest based DSLs.
 */
trait ManifestBased {
  type TypeRep[T] = Manifest[T]
  def runtimeType[T: TypeRep]: TypeRep[T] = manifest[T]
}

/**
 * BaseYinYang with ManifestBased.
 */
trait BaseYinYangManifest extends BaseYinYang with ManifestBased

/**
 * BaseYinYang with TypeTag.
 */
trait BaseYinYangTypeTag extends BaseYinYang with TypeTagBased
