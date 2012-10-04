package ch.epfl.lamp
package mpde

import scala.reflect.macros.Context
import language.experimental.macros

object MPDE {

  def lift[T, R](c: Context)(block: c.Expr[R]): c.Expr[T] = {
    import c.universe._
    
    object mpdeTransformer extends Transformer {

      override def transform(tree: Tree): Tree = {
        tree match {
          case Apply(rest, y) =>
            val result = super.transform(tree)
            println("s:" + rest + " y:" + y)
            result
          case _ =>
            super.transform(tree)
        }
      }
    }
    
    block.tree.foreach{
      case Apply(x, y) =>
        println(x + " --- " + y)
      case _ => ()
    }
    
     val t = mpdeTransformer.transform(block.tree)
    
    ???
  }
}
