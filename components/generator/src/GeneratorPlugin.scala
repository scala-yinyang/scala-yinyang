package ch.epfl.yinyang

import ch.epfl.lamp.autolifter._
import ch.epfl.lamp.autolifter.impl._
import scala.reflect.macros.whitebox.Context

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import scala.annotation.StaticAnnotation

/**
 * Annotation that marks that the class has a deep embedding.
 */
class deep extends StaticAnnotation

class BackendGenerator(val global: Global) extends Plugin {
  import global._

  val name = "backend-generator"
  val description = "Generates the deep DSL embedding based on the direct embedding."
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent {
    val global: BackendGenerator.this.global.type = BackendGenerator.this.global
    val runsAfter = List[String]("refchecks");
    val phaseName = BackendGenerator.this.name
    def newPhase(_prev: Phase) = new BackendGeneratorPhase(_prev)

    def liftClass(c: Context)(tpe: c.Type): Unit = {
      import c.universe._
      val al = new AutoLifter(c.universe)
      val lc = al.getLiftedClass(tpe)(annotations.Custom())
      val lp = al.getLiftedProgram(lc)
      val generator = new Generator {}
      println(generator.generate(lp))
    }

    class BackendGeneratorPhase(prev: Phase) extends StdPhase(prev) {
      override def name = BackendGenerator.this.name
      def apply(unit: CompilationUnit): Unit = {
        val PackageDef(_, defs) = unit.body
        val topLevelClasses = defs collect { case c: ClassDef => c }
        val classesForLifting = topLevelClasses filter { x =>
          x.symbol.annotations.exists(annot =>
            annot.tpe =:= typeOf[ch.epfl.yinyang.deep])
        }

        classesForLifting foreach { x =>
          liftClass(new {
            val universe: global.type = global
            val callsiteTyper: global.analyzer.Typer = global.typer
            val expandee = EmptyTree
          } with scala.reflect.macros.contexts.Context {
            val prefix = null
          })(x.symbol.tpe)
        }
      }

    }
  }
}