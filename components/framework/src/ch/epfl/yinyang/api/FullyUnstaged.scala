package ch.epfl.yinyang.api

import reflect.runtime.universe.Symbol

/**
 * Marker trait for DSLs in which no holes are used for optimizations, so all
 * holes are treated as constants instead of being lifted. This implies that
 * a code generating DSL will always be compiled at compile time.
 *
 * NOTE: DSLs that inherit this trait will not be reflectively instantiated
 * at compile time.
 */
trait FullyUnstaged { this: BaseYinYang =>

  override def requiredHoles(symbols: List[Symbol]): List[(Symbol, Guard)] =
    throw new RuntimeException("This method must not be called!!!")

}
