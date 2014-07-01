package ch.epfl.lamp
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
    generatePackage +
      generateImport +
      generateOps +
      generateExp(program) +
      generateGen(program) +
      generateImpl(program) +
      generateComp(program) +
      generateExpOpt(program)
  }

  def generatePackage(implicit program: LiftedProgram): String = {
    val fullName = program.actualOrigClass.toString.split("\\.")
    s"package ${fullName.take(fullName.length - 1).mkString(".")}\n"
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
    sb ++= "  " + program.objMethods.map(x => x.method + s" = ${x.method.name}_obj${x.method.argString}").mkString("  ", "\n  ", "\n")
    sb ++= s"  }\n"
    sb ++= s"  ${program.repClass.header} {\n"
    sb ++= program.opsMethods collect {
      case om: OpsMethod if om.repMethod.method.name == "hashCode" =>
        val rm = om.repMethod
        s"    ${rm.copy(method = rm.method.copy(name = "infix_hashCode"))} = ${om.method.call}"
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

  def generateExpSyms(implicit program: LiftedProgram): String = {
    program.caseClasses.collect({
      case mcc: MethodCaseClass if !mcc.functions.isEmpty =>
        s"case ${mcc.call} => syms(${mcc.params.head.variable.name}) ::: " + {
          mcc.functions.toList map { f =>
            s"syms(${f._2._2.variable.name})"
          } mkString " ::: "
        }
    }).mkString("\n    ")
  }

  def generateExpBoundSyms(implicit program: LiftedProgram): String = {
    program.caseClasses.collect({
      case mcc: MethodCaseClass if !mcc.functions.isEmpty =>
        s"case ${mcc.call} => " + {
          mcc.functions.toList map { f =>
            s"(${f._2._1.variable.name} :: effectSyms(${f._2._2.variable.name}))"
          } mkString " ::: "
        }
    }).mkString("\n    ")
  }

  def generateSymsFreq(implicit program: LiftedProgram): String = {
    program.caseClasses.collect({
      case mcc: MethodCaseClass if !mcc.functions.isEmpty =>
        s"case ${mcc.call} => freqNormal(${mcc.params.head.variable.name}) ::: " + {
          mcc.functions.toList map { f =>
            s"freqHot(${f._2._2.variable.name})"
          } mkString " ::: "
        }
    }).mkString("\n    ")
  }

  def generateExp(implicit program: LiftedProgram): String = {
    val sb = new StringBuilder
    sb ++= s"trait $expName extends $opsName with BaseExp with EffectExp with VariablesExp { this: $compName => \n"
    sb ++= "  // case classes\n"
    sb ++= program.caseClasses.mkString("  ", "\n  ", "\n")
    sb ++= "  // method definitions\n"
    sb ++= program.caseClasses.map(_.definition).mkString("  ", "\n  ", "\n")
    sb ++= s"""
  override def syms(e: Any): List[Sym[Any]] = e match {
    $generateExpSyms
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    $generateExpBoundSyms
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    $generateSymsFreq
    case _ => super.symsFreq(e)
  }
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
    sb ++= program.caseClasses.map(_.emit).mkString("\n    ", "\n    ", "")
    sb ++= """
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