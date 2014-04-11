package ch.epfl.yinyang
package transformers

import language.experimental.macros

import ch.epfl.yinyang._
import ch.epfl.yinyang.transformers._
import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.Universe
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

  private final class PostProcess extends Transformer {
    override def transform(tree: Tree): Tree = {
      tree match {
        case ClassDef(mods, name, tparams,
          Template(parents, self, body)) => {
          val List(constructor, DefDef(mainMods, mainName, mainTparams, mainVparamss,
            mainTpt, transformedBody)) = body collect { case d: DefDef => d }
          val valDefs = body collect { case v: ValDef => v }
          val classStatements = statementsWithContext(ClassContext)
          val mainStatements = statementsWithContext(MainContext)
          ClassDef(mods, name, tparams,
            Template(parents, self, classStatements ++ valDefs ++ List(constructor, DefDef(mainMods, mainName, mainTparams, mainVparamss,
              mainTpt, Block(mainStatements, transformedBody)))))
        }
      }
    }
  }
}

class NullPostProcessing[C <: Context](ctx: C) extends PostProcessing(ctx)(Nil) {
  import c.universe._
  override object PostProcess extends (Tree => Tree) {
    def apply(tree: Tree) = tree
  }
}

object PostProcessing {
  sealed trait StatementContext
  object ClassContext extends StatementContext
  object MainContext extends StatementContext
}