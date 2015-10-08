package ch.epfl.yinyang
package api
import scala.reflect.macros.blackbox.Context

protected[yinyang] class Reporter(private val c: Context) {
  import c.universe._
  def echo(pos: Position, msg: String): Unit = c.echo(pos, msg)
  def info(pos: Position, msg: String, force: Boolean): Unit = c.info(pos, msg, force)
  def warning(pos: Position, msg: String): Unit = c.warning(pos, msg)
  def error(pos: Position, msg: String): Unit = c.error(pos, msg)
}

/**
 * Trait that statically checked DSLs need to inherit
 *
 * NOTE: DSLs inheriting this trait will always be reflectively instantiated at compile time.
 */
trait StaticallyChecked { this: BaseYinYang =>

  /**
   * Method that implements domain-specific analysis and reports errors at compile time.
   */
  def staticallyCheck(c: Reporter)
}
