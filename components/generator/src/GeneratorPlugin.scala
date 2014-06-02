package ch.epfl.yinyang

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
        if (true) {
          unit.error(unit.body.pos, "Let's generate!")
        }
      }
    }
  }
}