package ch.epfl.data
package autolifter

import types._

trait Generator {
  def origName(implicit program: LiftedProgram) = program.origClass.name
  def opsName(implicit program: LiftedProgram) = s"${origName}Ops"
  def expName(implicit program: LiftedProgram) = s"${origName}Exp"
  def expOptName(implicit program: LiftedProgram) = s"${origName}ExpOpt"
  def genName(implicit program: LiftedProgram) = s"${origName}ScalaGen"
  def compName(implicit program: LiftedProgram) = s"${origName}Component"
  def implicitName(implicit program: LiftedProgram) = s"${origName}Implicits"

  def generate(implicit program: LiftedProgram): String = {
    generateImport +
      generateOps +
      generateExp(program) +
      generateGen(program) +
      generateImpl(program) +
      generateComp(program) +
      generateExpOpt(program)
  }

  def generateImport(implicit program: LiftedProgram): String = {
    """import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack

"""
  }

  def generateOps(implicit program: LiftedProgram): String = {
    val sb = new StringBuilder
    sb ++= s"trait $opsName extends Base with OverloadHack { this: $compName => \n"
    sb ++= s"  object $origName {\n"
    sb ++= "  " + program.objMethods.map(x => x.method + s" = ${x.method.copy(name = x.method.name + "_obj").call}").mkString("  ", "\n  ", "\n")
    sb ++= s"  }\n"
    sb ++= s"  ${program.repClass.header} {\n"
    sb ++= program.opsMethods collect {
      case om: OpsMethod if !om.repMethod.isConstructor =>
        val rm = om.repMethod
        s"    ${rm.toString} = ${om.method.call}"
    } mkString "\n"
    sb ++= "\n  }\n"
    sb ++= program.opsMethods.map(_.method).mkString("  ", "\n  ", "\n")
    sb ++= program.objMethods.map(x => x.copy(method = x.method.copy(name = x.method.name + "_obj")).method).mkString("  ", "\n  ", "\n")
    sb ++= s"  type ${program.origClass}\n"
    sb ++= "\n}\n"
    sb.toString
  }

  def generateExp(implicit program: LiftedProgram): String = {
    val sb = new StringBuilder
    sb ++= s"trait $expName extends $opsName with BaseExp with EffectExp with VariablesExp with LoweringTransformer with ArrayBufferOps with ArrayBufferOpsExp { this: $compName => \n"
    sb ++= "  // case classes\n"
    sb ++= program.caseClasses.mkString("  ", "\n  ", "\n")
    sb ++= "  // method definitions\n"
    sb ++= "  // lowered\n"
    sb ++= program.caseClasses.filterNot(x => x.opsMethod.method.originalSymbol.asMethod.isConstructor).map(_.loweredDefinition).mkString("  ", "\n  ", "\n")
    sb ++= "  // def\n"
    sb ++= program.caseClasses.filter(x => x.opsMethod.method.originalSymbol.asMethod.isConstructor).map(_.definition).mkString("  ", "\n  ", "\n")
    sb ++= "override def mirror[A:Manifest](e: Def[A], f$: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {\n"
    sb ++=
      program.caseClasses.filterNot(x => x.opsMethod.method.originalSymbol.asMethod.isConstructor)
      .map(_.mirror).mkString("  ", "\n  ", "\n")
    sb ++= """ case _ =>
      super.mirrorDef(e,f$)
  })
    """
    sb ++= s"  override type ${program.origClass} = ${program.actualOrigClass}\n"
    sb ++= "}"
    sb.toString
  }

  def generateGen(implicit program: LiftedProgram): String = {
    def caseClassToCase(caseClass: MethodCaseClass): String =
      s"""case ${caseClass.call} => emitValDef(sym, quote(v) + " * " + quote(k))"""
    val sb = new StringBuilder
    sb ++= s"""
trait $genName extends ScalaGenEffect {
  val IR: $compName
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {"""
    sb ++=
      program.caseClasses
      .filterNot(cc => cc.opsMethod.method.paramss.exists { p =>
        p.exists { x =>
          x.tpe.isFunction
        }
      })
      .map(_.emit).mkString("\n    ", "\n    ", "")
    sb ++= s"""
    case _ => super.emitNode(sym, node)
  }
}
"""
    sb.toString
  }

  def generateImpl(implicit program: LiftedProgram): String = {
    s"""trait $implicitName { this: $compName =>
  // Add implicit conversions here!
}
"""
  }

  def generateComp(implicit program: LiftedProgram): String = {
    s"""trait $compName extends $expOptName with $implicitName
    trait ${origName}DSL extends $compName with LMSCore { self =>
  val codegen = new QueryScalaGen with LMSCoreGen { val IR: self.type = self}
  val optimizer = new QueryTransformer { val IR: self.type = self }
}

trait ${origName}Transformer extends ForwardTransformer {
  val IR: $compName with LMSCore
  import IR._

  override def transformStm(stm: Stm): Exp[Any] = stm match {
    /* add global optimizations here */
    case _ => super.transformStm(stm)
  }
}
"""
  }

  def generateExpOpt(implicit program: LiftedProgram): String = {
    val methods = program.opsMethods.map(rm => {
      val m = rm.method
      s"""override $m = {
    /* add local optimizations here */
    super.${m.call}
  }"""

    }).mkString("", "\n  ", "\n")
    s"""trait $expOptName extends $expName { this: $compName =>
  $methods
}
"""
  }
}