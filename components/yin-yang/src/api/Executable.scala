package ch.epfl.yinyang.api

/**
 * Base trait for executable DSLs. Yin-Yang will pass the arguments
 * by calling q"dsl.execute[$retType](..$sortedArguments)"
 */
trait Executable { self: BaseYinYang => }
