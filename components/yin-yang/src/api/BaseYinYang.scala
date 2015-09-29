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
