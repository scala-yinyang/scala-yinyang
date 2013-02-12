package dsl.print

import ch.epfl.lamp.mpde._
import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.tools.reflect.ToolBoxFactory

object `package` {

  // Debugging aids
  private[this] final val DEBUG_PRINT_DSL = false

  def liftPrint[T](block: ⇒ T): Any = macro _liftPrint[T]
  def _liftPrint[T](c: Context)(block: c.Expr[T]): c.Expr[T] = {
    import c._
    import c.universe._

    def safe_println(x: Any) = if (DEBUG_PRINT_DSL) scala.Predef.println(x) else ()

    safe_println("********************************************************************************")
    safe_println("Lifting:")
    safe_println(block)

    // Staging workflow:
    //
    //  program code (block) ==> lifted code ==> staged code ==> parsed trees
    //
    // if there are external dependencies, like in:
    //
    // val x = 3
    // liftPrint { print(x + 4) }
    //
    // the code can't be evaluated and thus we can only place the lifted code
    // back in the program and expect to perform interpretation at runtime
    try {
      val lifted = new MPDETransformer[c.type, T](c, "dsl.print.PrintDSLStage")(block)
      val staged = c.eval(lifted).asInstanceOf[String] // we know it returns a string, this is how we defined interpret
      val parsed = c.parse(staged)
      safe_println("Static staging (no external dependencies):")
      safe_println(parsed)
      safe_println("********************************************************************************")
      c.Expr(parsed)
    } catch {
      case e: scala.tools.reflect.ToolBoxError ⇒
        safe_println("\nCode has external dependencies:")
        safe_println(e.getStackTrace.mkString("  ", "\n  ", "\n"))
        val lifted = new MPDETransformer[c.type, T](c, "dsl.print.PrintDSLInterpret")(block)
        safe_println("\nWill use interpretation (since code block HAS external dependencies):")
        safe_println(lifted)
        safe_println("********************************************************************************")
        lifted
    }
  }

  // The only thing we declare here
  def println(x: Any) = ???
  def returns(x: Any) = ??? // added because we cannot test output

  // Still, we need to be able to print:
  def safe_println(x: Any) = scala.Predef.println(x)
}


/**
 *  val canStatic = analyze(block)
 *  if (canStatic) 
 *     // do the above but in the smarter way
 *  else 
 *     // postpone to runtime like it is done before
 * 
 * 
 * In the DSL: 
 *   def match(text: String, regex: String @required)
 * 
 * 1)
 * analyze(block) 
 *  * true if regex is known at compile time across the whole DSL
 *     * match(variable, "abc*") 
 *  * false otherwise
 * 
 * 2)Code that we generate needs to link to variables. E.g:
 *   automaton.match(variable)
 *    
 *    i) Staged code= 
 *         object uniqueId {
 *            def method(p1: String, ...) = {
 *              // generated code that uses p1 instead of variable
 *            }
 *         }
 *         method(variable, ...)
 *    ii) relink the variables in the generated code
 *       val parsed = ...
 *       transform(parsed)
 *       transform will do the relinking 
 * 
 */
