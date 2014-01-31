package ch.epfl.yinyang.api

import reflect.runtime.universe.Symbol

/**
 * Marker trait for DSLs in which all holes are used for optimizations and
 * therefore lifted. This implies that if a DSL program has at least one hole,
 * it won't be compiled at compile time. All variables are treated as
 * [[RequiredStaticCompVar]] with the default guard.
 *
 * NOTE: DSLs that inherit this trait will not be reflectively instantiated
 * at compile time.
 */
trait FullyStaged { this: BaseYinYang =>

  override def compilationVars(symbols: List[Symbol]): List[VarType] =
    throw new RuntimeException("This method must not be called!!!")

}
