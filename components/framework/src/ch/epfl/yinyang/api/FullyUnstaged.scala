package ch.epfl.yinyang.api

import reflect.runtime.universe.Symbol

/**
 * Marker trait for DSLs in which no holes are used for optimizations, so all
 * holes are treated as constants instead of being lifted. This implies that
 * a code generating DSL will always be compiled at compile time. In other
 * words, all variables are treated as [[NonCompVar]].
 *
 * NOTE: DSLs that inherit this trait will not be reflectively instantiated
 * at compile time.
 */
trait FullyUnstaged { this: BaseYinYang =>

  override def compilationVars(symbols: List[Symbol]): List[VarType] =
    throw new RuntimeException("This method must not be called!!!")

}
