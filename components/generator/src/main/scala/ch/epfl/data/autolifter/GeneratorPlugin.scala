package ch.epfl.data
package autolifter

import ch.epfl.data.autolifter._
import ch.epfl.data.autolifter.impl._
import scala.reflect.macros.whitebox.Context

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import scala.annotation.StaticAnnotation

import java.io._

class GeneratorPlugin(val global: Global) extends Plugin {
  import global._

  val name = "backend-generator"
  val description = "Generates the deep DSL embedding based on the direct embedding."
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent {
    val global: GeneratorPlugin.this.global.type = GeneratorPlugin.this.global
    val runsAfter = List[String]("refchecks");
    val phaseName = GeneratorPlugin.this.name
    def newPhase(_prev: Phase) = new GeneratorPhase(_prev)

    def liftClass(c: Context)(classDef: c.universe.ClassDef, component: String): String = {
      import c.universe._
      val al = new AutoLifter(c.universe)
      val config = annotations.Custom(component)
      val liftedClass =
        al.getLiftedClass(classDef.symbol.info)(annotations.Custom(component))
      val liftedModule =
        al.getLiftedModule(classDef.symbol.info)(annotations.Custom(component))

      // populate bodies
      val methods = liftedClass.methods
      val methodsWithBodies = methods.map { m =>
        val body = classDef match {
          case ClassDef(_, _, _, template) =>
            template.body collectFirst {
              case dd @ DefDef(_, _, _, _, _, mbody) if m.symbol == dd.symbol =>
                mbody
            }
        }
        import autolifter.types._
        import impl.ImplLifter._
        import ch.epfl.yinyang.typetransformers._
        val implLifter = new ImplLifter[c.type](c)("", Map()) {
          val typeTransformer = new RepTransformer[c.type](c)
          typeTransformer.className = ""
        }
        //
        val transformedBody = body.filter(_ != EmptyTree).map { body =>
          val bodyCopy = body.duplicate
          val tpe = Type(liftedClass.tpe.typeSymbol)
          val preproc = new PreProcessor[c.type](c)(tpe, TypeContext(List(tpe), Nil)) {}
          val processBody = preproc.PreProcess(implLifter(bodyCopy))

          val pp = new PostProcessor[c.type](c)(tpe, TypeContext(List(tpe), Nil)) {}
          val res = pp.PostProcess(implLifter(processBody))
          showCode(res)
        }

        m.copy(body = transformedBody)
      }

      val classWithBodies = liftedClass.copy(methods = methodsWithBodies)
      val prog = al.getLiftedProgram(classWithBodies, liftedModule, component)
      al.generate(prog)
    }

    class GeneratorPhase(prev: Phase) extends StdPhase(prev) {
      override def name = GeneratorPlugin.this.name
      def apply(unit: CompilationUnit): Unit = {

        def topLevelClasses(body: Tree): List[ClassDef] = body match {
          case PackageDef(_, defs) => defs.flatMap(topLevelClasses)
          case c: ClassDef         => List(c)
          case _                   => Nil
        }

        val classesForLifting = topLevelClasses(unit.body) filter { x =>
          x.symbol.annotations.exists(annot =>
            annot.tpe =:= typeOf[ch.epfl.data.autolifter.annotations.deep])
        }

        // This is the current solution to pass the necessary data that should be computed
        val metaData: List[ClassDef] = topLevelClasses(unit.body) filter { x =>
          x.symbol.annotations.exists(annot =>
            annot.tpe =:= typeOf[ch.epfl.data.autolifter.annotations.metadeep])
        }

        if (metaData.size == 1 && classesForLifting.size > 0) { // all OK
          global.reporter.info(NoPosition,
            s"Generating for classes ${classesForLifting.map(_.name).mkString("[", ", ", "]")} in file ${unit.source.file}",
            force = true)

          val metaAnnotation =
            metaData.head.symbol.annotations.filter(_.tpe =:= typeOf[ch.epfl.data.autolifter.annotations.metadeep]).head
          val (folder, imports, component) = metaAnnotation match {
            case AnnotationInfo(x, args, z) =>
              val params = args.map { case Literal(Constant(l)) => l.toString }
              (params(0), params(1), params(2))
          }

          val liftedClasses = classesForLifting map { classDef =>
            // here we make a macro context in order to use the Macro API
            val liftedClass = liftClass(new {
              val universe: global.type = global
              val callsiteTyper: global.analyzer.Typer = global.typer
              val expandee = EmptyTree
            } with scala.reflect.macros.contexts.Context {
              val prefix = null
            })(classDef, component)

            liftedClass
          }

          val out = new PrintWriter(new File(new File(folder), unit.source.file.name))
          out.write(imports)
          liftedClasses.foreach(out.write)
          out.flush
        } else if (classesForLifting.size > 0) {
          unit.error(NoPosition, "File that contains @deep annotations must also contain exactly one @metadeep annotation.")
        }
      }
    }
  }
}