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
    sb ++= s"trait $expName extends $opsName with BaseExp with EffectExp with VariablesExp with LoweringTransformer { this: $compName => \n"
    sb ++= "  // case classes\n"
    sb ++= program.caseClasses.mkString("  ", "\n  ", "\n")
    sb ++= "  // method definitions\n"
    sb ++= program.caseClasses.filterNot(x => x.opsMethod.method.originalSymbol.asMethod.isConstructor).map(_.loweredDefinition).mkString("  ", "\n  ", "\n")
    sb ++= program.caseClasses.filter(x => x.opsMethod.method.originalSymbol.asMethod.isConstructor).map(_.definition).mkString("  ", "\n  ", "\n")
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
    sb ++= program.caseClasses.map(_.emit).mkString("\n    ", "\n    ", "")
    sb ++= s"""
    case _ => super.emitNode(sym, node)
  }
  /*
    ${program.caseClasses.map(_.opsMethod.method.body)}
  */
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
"""
  }

  def generateExpOpt(implicit program: LiftedProgram): String = {
    val methods = program.opsMethods.map(rm => {
      val m = rm.method
      s"""override $m = {
    /* add optimization here */
    super.${m.call}
  }"""

    }).mkString("", "\n  ", "\n")
    s"""trait $expOptName extends $expName { this: $compName =>
  $methods
}
"""
  }
}