package ch.epfl.lamp
package mpde

import scala.reflect.macros.Context
import language.experimental.macros

object MPDE {

  /**
   * Macro that converts pure DSL embedding into a deep one. For now it simply prints the trees.
   */
  def lift[T](c: Context)(block: c.Expr[T]): c.Expr[T] = {
    import c.universe._
    
    object mpdeTransformer extends Transformer {

      override def transform(tree: Tree): Tree = {
        tree match {
          case Apply(rest, y) =>
            val result = super.transform(tree)
            // prints all applications
            println("r: " + rest + " y: " + y)
            result
          case _ =>
            super.transform(tree)
        }
      }
    }
    
    // simply prints the tree
    block.tree.foreach{
      case Apply(x, y) =>
        println(x + " --- " + y)
      case _ => ()
    }
    
    val t = mpdeTransformer.transform(block.tree)
    
    // simply return the original tree
    block
  }
  
}
