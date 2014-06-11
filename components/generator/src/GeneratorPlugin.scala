package ch.epfl.yinyang

import ch.epfl.lamp.autolifter._
import ch.epfl.lamp.autolifter.impl._
import scala.reflect.macros.whitebox.Context

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

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

    class BackendGeneratorPhase(prev: Phase) extends StdPhase(prev) {
      override def name = BackendGenerator.this.name
      def apply(unit: CompilationUnit): Unit = {
        val PackageDef(_, defs) = unit.body
        val topLevelClasses = defs collect { case c: ClassDef => c }
        val classesForLifting = topLevelClasses filter { x =>
          x.symbol.annotations.exists(annot =>
            annot.tpe =:= typeOf[scala.deprecated])
          // TODO introduce our own annotation
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

        if (true) {
          unit.error(unit.body.pos, "Let's generate!")
        }
      }

      def liftClass(c: Context)(tpe: c.Type): Unit = {
        import c.universe._
        val mh = new MacroHelper[c.type](c)
        println("Hey!")
        val liftedMethods = mh.liftClass(tpe)

        liftedMethods foreach {
          case (s, m) =>
            println(s)
            showCode(m)
        }

        val al = new AutoLifter(c.universe)
        val lc = al.getLiftedClass(tpe)(annotations.Custom())
        val lp = al.getLiftedProgram(lc)
        val liftedMethodsMap = liftedMethods.toMap
        lp.caseClasses foreach { cc =>
          val sym = cc.opsMethod.method.originalSymbol.asInstanceOf[Symbol]
          println(cc + "->" + showCode(liftedMethodsMap(sym)))
        }
      }

    }
  }
}