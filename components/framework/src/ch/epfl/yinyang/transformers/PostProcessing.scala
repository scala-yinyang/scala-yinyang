package ch.epfl.yinyang
package transformers

import ch.epfl.yinyang._
import ch.epfl.yinyang.transformers._
import scala.reflect.macros.Context
import scala.reflect.macros.Universe
import language.experimental.macros
import scala.collection.mutable

/**
 * Post processing phase, in which you can add more statements to final DSL. Its main use-case is for class virtualization.
 */
class PostProcessing[C <: Context](val c: C)(val statements: List[(PostProcessing.StatementContext, Universe#Tree)]) {
  import c.universe._
  import PostProcessing._

  @inline def statementsWithContext(statementContext: StatementContext): List[Tree] = {
    statements.filter(_._1 == statementContext).map(_._2).asInstanceOf[List[Tree]]
  }

  val PostProcess = new (Tree => Tree) {
    def apply(tree: Tree) = new PostProcess().transform(tree)
  }
  // def apply(tree: Tree): Tree = transform(tree)

  private final class PostProcess extends Transformer {
    override def transform(tree: Tree): Tree = {
      tree match {
        case ClassDef(mods, name, tparams,
          Template(parents, self, List(constructor, DefDef(mainMods, mainName, mainTparams, mainVparamss,
            mainTpt, transformedBody)))) => {
          // val classStatements = statements.filter(_._1 == ClassContext).map(_._2)
          // val mainStatements = statements.filter(_._1 == MainContext).map(_._2)
          val classStatements = statementsWithContext(ClassContext)
          val mainStatements = statementsWithContext(MainContext)
          // val newTransformed = FunctionAscriptionRemove(transformedBody)
          ClassDef(mods, name, tparams,
            Template(parents, self, classStatements ++ List(constructor, DefDef(mainMods, mainName, mainTparams, mainVparamss,
              mainTpt, Block(mainStatements, transformedBody)))))
        }
      }
    }
  }
  // override def transform(tree: Tree): Tree = {
  //   tree match {
  //     case ClassDef(mods, name, tparams,
  //       Template(parents, self, List(constructor, DefDef(mainMods, mainName, mainTparams, mainVparamss,
  //         mainTpt, transformedBody)))) => {
  //       val classStatements = statements.filter(_._1 == ClassContext).map(_._2)
  //       val mainStatements = statements.filter(_._1 == MainContext).map(_._2)
  //       ClassDef(mods, name, tparams,
  //         Template(parents, self, classStatements ++ List(constructor, DefDef(mainMods, mainName, mainTparams, mainVparamss,
  //           mainTpt, Block(mainStatements, transformedBody)))))
  //     }
  //   }
  // }

  //   object FunctionAscriptionRemove extends Transformer with (Tree => Tree) {
  //     def apply(tree: Tree): Tree = transform(tree)

  // override def transform(tree: Tree): Tree = {
  //       tree match {
  //         case fun @ Function(vparams, body) => {
  //           log(("=" * 10) + s"closure found: $fun")
  //           val newVparams = vparams map {
  //             case ValDef(mods, name, tpt, rhs) => {
  //               log(c.universe.showRaw(tpt))
  //               // log(c.universe.showRaw(tpt.tpe))
  //               // def containsVirtualizedTpt()
  //               val newTpt = tpt match {
  //                 case Select(_, name) if virtualizedClassNames.contains(name.toString) =>
  //                   TypeTree()
  //                 // case AppliedTypeTree(_, tpts) => {
  //                 //   log("arguments:")
  //                 //   tpts.foreach(x => log(c.universe.showRaw(x)))
  //                 //   tpts.foreach(x => log(c.universe.showRaw(x.tpe)))
  //                 //   tpt
  //                 // }
  //                 case _ =>
  //                   transform(tpt)
  //               }

  //               // val newTpt =
  //               //   if (tpt.tpe != null && isVirtualizedType(tpt.tpe))
  //               //     Ident("Any")
  //               //   else
  //               //     tpt
  //               ValDef(mods, name, newTpt, rhs)
  //             }
  //           }
  //           Function(newVparams, transform(body))
  //         }
  //         case _ => super.transform(tree)
  //       }
  //     }
  //   }
}

class NullPostProcessing[C <: Context](ctx: C) extends PostProcessing(ctx)(Nil) {
  import c.universe._
  // override def transform(tree: Tree): Tree = tree
  override object PostProcess extends (Tree => Tree) {
    def apply(tree: Tree) = tree
  }
}

object PostProcessing {
  sealed trait StatementContext
  object ClassContext extends StatementContext
  object MainContext extends StatementContext

  // type Statement[C <: Context] = (StatementContext, C#Tree)
}