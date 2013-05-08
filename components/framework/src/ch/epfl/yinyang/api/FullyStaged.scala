package ch.epfl.yinyang.api

/**
 * Marker trait for DSLs in which all holes are treated as constants.
 *
 * NOTE: DSLs that inherit this trait will not be reflectively instantiated at compile time.
 */
trait FullyStaged { this: BaseYinYang =>

  override def requiredHoles: List[Int] =
    throw new RuntimeException("This method must not be called!!!")

}
