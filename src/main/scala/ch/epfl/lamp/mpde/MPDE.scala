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
    //println("Block:" + showRaw(block, printIds = true, printTypes = true))
    object mpdeTransformer extends Transformer {

      override def transform(tree: Tree): Tree = {
        tree match {
          case Apply(TypeApply(Select(tpe, method), targs), args) =>
            // prints all applications            
            treeCopy.Apply(tree, TypeApply(Select(tpe, method + "_rep"), targs), args.map(transform(_)))
          //case t @ ValDef(mods, name, tpt, rhs) =>
            //treeCopy.ValDef(t, mods, name, tpt, transform(rhs))
          case _ =>            
            super.transform(tree)
        }
      }
    }
    
    val res = c.resetAllAttrs(mpdeTransformer.transform(block.tree))
    // println(showRaw(res, printIds = true, printTypes = true))
    // println(show(res1))    
    val res1 = c.Expr[T](res)

    res1 
  }
  
}
