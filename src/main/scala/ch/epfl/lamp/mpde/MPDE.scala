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
          case Apply(TypeApply(Select(tpe, method), _), y) =>
            // How to get the trait where this method is defined?
            // How to cover the chain of applications?
            
            // if (rep method exists) 
            //   push trees that need to be converted on a stack
            //   recurse to them 
            
            // prints all applications            
            println(s"tpe: $tpe method: $method y: $y")

            val result = super.transform(tree)
            
            
            // if (the stack marks that this tree should be converted)
            //   find the conversion method
            
            result
          
          case _ =>
            super.transform(tree)
        }
      }
    }
    
    // simply prints the tree
    block.tree.foreach {
      case Apply(x, y) =>
        println(x + " --- " + y)
      case _ => ()
    }
    
    val t = mpdeTransformer.transform(block.tree)
    
    // simply return the original tree
    block
  }
  
}
