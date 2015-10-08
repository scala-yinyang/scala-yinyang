package ch.epfl.yinyang.api

import reflect.runtime.universe._

/**
 * Base trait for all YinYang based DSLs. The sole reason for its existence
 * is the existence of dual type representation.
 */
trait BaseYinYang {
  /**
   * Abstraction over different scala runtime types. Once the Manifests are deprecated
   * this abstraction will be removed.
   */
  type TypeRep[T]
  def $tpe[T: TypeRep]: TypeRep[T]

}

/*
 * Component for TypeTag based DSLs.
 */
trait BaseYinYangTypeTag extends BaseYinYang {
  type TypeRep[T] = TypeTag[T]
  def $tpe[T: TypeRep]: TypeRep[T] = typeTag[T]
}

/*
 * Component for Manifest based DSLs.
 */
trait BaseYinYangManifest extends BaseYinYang {
  type TypeRep[T] = Manifest[T]
  def $tpe[T: TypeRep]: TypeRep[T] = manifest[T]
}
