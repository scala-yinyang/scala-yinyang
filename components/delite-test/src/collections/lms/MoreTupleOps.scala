package collections.lms

import java.io.PrintWriter

import scala.language.implicitConversions

import scala.reflect.SourceContext

import scala.virtualization.lms.common.{
  TupleOps,
  TupleOpsExp,
  ScalaGenTupleOps
}

trait MoreTupleOps extends TupleOps {
  implicit def make_tuple9[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, A5: Manifest, A6: Manifest, A7: Manifest, A8: Manifest, A9: Manifest](t: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9]))(implicit pos: SourceContext): Rep[(A1, A2, A3, A4, A5, A6, A7, A8, A9)]
  implicit def make_tuple16[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, A5: Manifest, A6: Manifest, A7: Manifest, A8: Manifest, A9: Manifest, A10: Manifest, A11: Manifest, A12: Manifest, A13: Manifest, A14: Manifest, A15: Manifest, A16: Manifest](t: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5], Rep[A6], Rep[A7], Rep[A8], Rep[A9], Rep[A10], Rep[A11], Rep[A12], Rep[A13], Rep[A14], Rep[A15], Rep[A16]))(
    implicit pos: SourceContext): Rep[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)]

  implicit def t9[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, A5: Manifest, A6: Manifest, A7: Manifest, A8: Manifest, A9: Manifest](t: Rep[(A1, A2, A3, A4, A5, A6, A7, A8, A9)])(implicit pos: SourceContext) =
    ((tuple9_get1(t), tuple9_get2(t), tuple9_get3(t), tuple9_get4(t),
      tuple9_get5(t), tuple9_get6(t), tuple9_get7(t), tuple9_get8(t),
      tuple9_get9(t)))
  implicit def t16[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, A5: Manifest, A6: Manifest, A7: Manifest, A8: Manifest, A9: Manifest, A10: Manifest, A11: Manifest, A12: Manifest, A13: Manifest, A14: Manifest, A15: Manifest, A16: Manifest](t: Rep[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)])(implicit pos: SourceContext) =
    ((tuple16_get1(t), tuple16_get2(t), tuple16_get3(t), tuple16_get4(t),
      tuple16_get5(t), tuple16_get6(t), tuple16_get7(t), tuple16_get8(t),
      tuple16_get9(t), tuple16_get10(t), tuple16_get11(t), tuple16_get12(t),
      tuple16_get13(t), tuple16_get14(t), tuple16_get15(t), tuple16_get16(t)))

  def tuple9_get1[A: Manifest](t: Rep[(A, _, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple9_get2[A: Manifest](t: Rep[(_, A, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple9_get3[A: Manifest](t: Rep[(_, _, A, _, _, _, _, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple9_get4[A: Manifest](t: Rep[(_, _, _, A, _, _, _, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple9_get5[A: Manifest](t: Rep[(_, _, _, _, A, _, _, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple9_get6[A: Manifest](t: Rep[(_, _, _, _, _, A, _, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple9_get7[A: Manifest](t: Rep[(A, _, _, _, _, _, A, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple9_get8[A: Manifest](t: Rep[(_, _, _, _, _, _, _, A, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple9_get9[A: Manifest](t: Rep[(_, _, _, _, _, _, _, _, A)])(
    implicit pos: SourceContext): Rep[A]

  def tuple16_get1[A: Manifest](t: Rep[(A, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple16_get2[A: Manifest](t: Rep[(_, A, _, _, _, _, _, _, _, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple16_get3[A: Manifest](t: Rep[(_, _, A, _, _, _, _, _, _, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple16_get4[A: Manifest](t: Rep[(_, _, _, A, _, _, _, _, _, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple16_get5[A: Manifest](t: Rep[(_, _, _, _, A, _, _, _, _, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple16_get6[A: Manifest](t: Rep[(_, _, _, _, _, A, _, _, _, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple16_get7[A: Manifest](t: Rep[(_, _, _, _, _, _, A, _, _, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple16_get8[A: Manifest](t: Rep[(_, _, _, _, _, _, _, A, _, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple16_get9[A: Manifest](t: Rep[(_, _, _, _, _, _, _, _, A, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple16_get10[A: Manifest](t: Rep[(_, _, _, _, _, _, _, _, _, A, _, _, _, _, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple16_get11[A: Manifest](t: Rep[(_, _, _, _, _, _, _, _, _, _, A, _, _, _, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple16_get12[A: Manifest](t: Rep[(_, _, _, _, _, _, _, _, _, _, _, A, _, _, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple16_get13[A: Manifest](t: Rep[(_, _, _, _, _, _, _, _, _, _, _, _, A, _, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple16_get14[A: Manifest](t: Rep[(_, _, _, _, _, _, _, _, _, _, _, _, _, A, _, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple16_get15[A: Manifest](t: Rep[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, A, _)])(
    implicit pos: SourceContext): Rep[A]
  def tuple16_get16[A: Manifest](t: Rep[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A)])(
    implicit pos: SourceContext): Rep[A]
}

trait MoreTupleOpsExp extends MoreTupleOps with TupleOpsExp {

  implicit def make_tuple9[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, A5: Manifest, A6: Manifest, A7: Manifest, A8: Manifest, A9: Manifest](t: (Exp[A1], Exp[A2], Exp[A3], Exp[A4], Exp[A5], Exp[A6], Exp[A7], Exp[A8], Exp[A9]))(implicit pos: SourceContext): Exp[(A1, A2, A3, A4, A5, A6, A7, A8, A9)] = ETuple9(
    t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)
  implicit def make_tuple16[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, A5: Manifest, A6: Manifest, A7: Manifest, A8: Manifest, A9: Manifest, A10: Manifest, A11: Manifest, A12: Manifest, A13: Manifest, A14: Manifest, A15: Manifest, A16: Manifest](t: (Exp[A1], Exp[A2], Exp[A3], Exp[A4], Exp[A5], Exp[A6], Exp[A7], Exp[A8], Exp[A9], Exp[A10], Exp[A11], Exp[A12], Exp[A13], Exp[A14], Exp[A15], Exp[A16]))(
    implicit pos: SourceContext): Exp[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] = ETuple16(
    t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8,
    t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)

  case class ETuple9[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, A5: Manifest, A6: Manifest, A7: Manifest, A8: Manifest, A9: Manifest](
    _1: Exp[A1], _2: Exp[A2], _3: Exp[A3], _4: Exp[A4],
    _5: Exp[A5], _6: Exp[A6], _7: Exp[A7], _8: Exp[A8],
    _9: Exp[A9]) extends Def[(A1, A2, A3, A4, A5, A6, A7, A8, A9)] {
    val m1 = manifest[A1]
    val m2 = manifest[A2]
    val m3 = manifest[A3]
    val m4 = manifest[A4]
    val m5 = manifest[A5]
    val m6 = manifest[A6]
    val m7 = manifest[A7]
    val m8 = manifest[A8]
    val m9 = manifest[A9]
  }
  case class ETuple16[A1: Manifest, A2: Manifest, A3: Manifest, A4: Manifest, A5: Manifest, A6: Manifest, A7: Manifest, A8: Manifest, A9: Manifest, A10: Manifest, A11: Manifest, A12: Manifest, A13: Manifest, A14: Manifest, A15: Manifest, A16: Manifest](
    _1: Exp[A1], _2: Exp[A2], _3: Exp[A3], _4: Exp[A4],
    _5: Exp[A5], _6: Exp[A6], _7: Exp[A7], _8: Exp[A8],
    _9: Exp[A9], _10: Exp[A10], _11: Exp[A11], _12: Exp[A12],
    _13: Exp[A13], _14: Exp[A14], _15: Exp[A15], _16: Exp[A16]) extends Def[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] {
    val m1 = manifest[A1]
    val m2 = manifest[A2]
    val m3 = manifest[A3]
    val m4 = manifest[A4]
    val m5 = manifest[A5]
    val m6 = manifest[A6]
    val m7 = manifest[A7]
    val m8 = manifest[A8]
    val m9 = manifest[A9]
    val m10 = manifest[A10]
    val m11 = manifest[A11]
    val m12 = manifest[A12]
    val m13 = manifest[A13]
    val m14 = manifest[A14]
    val m15 = manifest[A15]
    val m16 = manifest[A16]
  }

  case class Tuple9Access1[A: Manifest](t: Exp[(A, _, _, _, _, _, _, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple9Access2[A: Manifest](t: Exp[(_, A, _, _, _, _, _, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple9Access3[A: Manifest](t: Exp[(_, _, A, _, _, _, _, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple9Access4[A: Manifest](t: Exp[(_, _, _, A, _, _, _, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple9Access5[A: Manifest](t: Exp[(_, _, _, _, A, _, _, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple9Access6[A: Manifest](t: Exp[(_, _, _, _, _, A, _, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple9Access7[A: Manifest](t: Exp[(A, _, _, _, _, _, A, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple9Access8[A: Manifest](t: Exp[(_, _, _, _, _, _, _, A, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple9Access9[A: Manifest](t: Exp[(_, _, _, _, _, _, _, _, A)])
    extends Def[A] { val m = manifest[A] }

  case class Tuple16Access1[A: Manifest](t: Exp[(A, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple16Access2[A: Manifest](t: Exp[(_, A, _, _, _, _, _, _, _, _, _, _, _, _, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple16Access3[A: Manifest](t: Exp[(_, _, A, _, _, _, _, _, _, _, _, _, _, _, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple16Access4[A: Manifest](t: Exp[(_, _, _, A, _, _, _, _, _, _, _, _, _, _, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple16Access5[A: Manifest](t: Exp[(_, _, _, _, A, _, _, _, _, _, _, _, _, _, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple16Access6[A: Manifest](t: Exp[(_, _, _, _, _, A, _, _, _, _, _, _, _, _, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple16Access7[A: Manifest](t: Exp[(_, _, _, _, _, _, A, _, _, _, _, _, _, _, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple16Access8[A: Manifest](t: Exp[(_, _, _, _, _, _, _, A, _, _, _, _, _, _, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple16Access9[A: Manifest](t: Exp[(_, _, _, _, _, _, _, _, A, _, _, _, _, _, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple16Access10[A: Manifest](t: Exp[(_, _, _, _, _, _, _, _, _, A, _, _, _, _, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple16Access11[A: Manifest](t: Exp[(_, _, _, _, _, _, _, _, _, _, A, _, _, _, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple16Access12[A: Manifest](t: Exp[(_, _, _, _, _, _, _, _, _, _, _, A, _, _, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple16Access13[A: Manifest](t: Exp[(_, _, _, _, _, _, _, _, _, _, _, _, A, _, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple16Access14[A: Manifest](t: Exp[(_, _, _, _, _, _, _, _, _, _, _, _, _, A, _, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple16Access15[A: Manifest](t: Exp[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, A, _)])
    extends Def[A] { val m = manifest[A] }
  case class Tuple16Access16[A: Manifest](t: Exp[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A)])
    extends Def[A] { val m = manifest[A] }

  def tuple9_get1[A: Manifest](t: Exp[(A, _, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple9(a, _, _, _, _, _, _, _, _)) ⇒ a
    case _                                       ⇒ Tuple9Access1(t)
  }
  def tuple9_get2[A: Manifest](t: Exp[(_, A, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple9(_, a, _, _, _, _, _, _, _)) ⇒ a
    case _                                       ⇒ Tuple9Access2(t)
  }
  def tuple9_get3[A: Manifest](t: Exp[(_, _, A, _, _, _, _, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple9(_, _, a, _, _, _, _, _, _)) ⇒ a
    case _                                       ⇒ Tuple9Access3(t)
  }
  def tuple9_get4[A: Manifest](t: Exp[(_, _, _, A, _, _, _, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple9(_, _, _, a, _, _, _, _, _)) ⇒ a
    case _                                       ⇒ Tuple9Access4(t)
  }
  def tuple9_get5[A: Manifest](t: Exp[(_, _, _, _, A, _, _, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple9(_, _, _, _, a, _, _, _, _)) ⇒ a
    case _                                       ⇒ Tuple9Access5(t)
  }
  def tuple9_get6[A: Manifest](t: Exp[(_, _, _, _, _, A, _, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple9(_, _, _, _, _, a, _, _, _)) ⇒ a
    case _                                       ⇒ Tuple9Access6(t)
  }
  def tuple9_get7[A: Manifest](t: Exp[(A, _, _, _, _, _, A, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple9(_, _, _, _, _, _, a, _, _)) ⇒ a
    case _                                       ⇒ Tuple9Access7(t)
  }
  def tuple9_get8[A: Manifest](t: Exp[(_, _, _, _, _, _, _, A, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple9(_, _, _, _, _, _, _, a, _)) ⇒ a
    case _                                       ⇒ Tuple9Access8(t)
  }
  def tuple9_get9[A: Manifest](t: Exp[(_, _, _, _, _, _, _, _, A)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple9(_, _, _, _, _, _, _, _, a)) ⇒ a
    case _                                       ⇒ Tuple9Access9(t)
  }

  def tuple16_get1[A: Manifest](t: Exp[(A, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple16(a, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)) ⇒ a
    case _ ⇒ Tuple16Access1(t)
  }
  def tuple16_get2[A: Manifest](t: Exp[(_, A, _, _, _, _, _, _, _, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple16(_, a, _, _, _, _, _, _, _, _, _, _, _, _, _, _)) ⇒ a
    case _ ⇒ Tuple16Access2(t)
  }
  def tuple16_get3[A: Manifest](t: Exp[(_, _, A, _, _, _, _, _, _, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple16(_, _, a, _, _, _, _, _, _, _, _, _, _, _, _, _)) ⇒ a
    case _ ⇒ Tuple16Access3(t)
  }
  def tuple16_get4[A: Manifest](t: Exp[(_, _, _, A, _, _, _, _, _, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple16(_, _, _, a, _, _, _, _, _, _, _, _, _, _, _, _)) ⇒ a
    case _ ⇒ Tuple16Access4(t)
  }
  def tuple16_get5[A: Manifest](t: Exp[(_, _, _, _, A, _, _, _, _, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple16(_, _, _, _, a, _, _, _, _, _, _, _, _, _, _, _)) ⇒ a
    case _ ⇒ Tuple16Access5(t)
  }
  def tuple16_get6[A: Manifest](t: Exp[(_, _, _, _, _, A, _, _, _, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple16(_, _, _, _, _, a, _, _, _, _, _, _, _, _, _, _)) ⇒ a
    case _ ⇒ Tuple16Access6(t)
  }
  def tuple16_get7[A: Manifest](t: Exp[(_, _, _, _, _, _, A, _, _, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple16(_, _, _, _, _, _, a, _, _, _, _, _, _, _, _, _)) ⇒ a
    case _ ⇒ Tuple16Access7(t)
  }
  def tuple16_get8[A: Manifest](t: Exp[(_, _, _, _, _, _, _, A, _, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple16(_, _, _, _, _, _, _, a, _, _, _, _, _, _, _, _)) ⇒ a
    case _ ⇒ Tuple16Access8(t)
  }
  def tuple16_get9[A: Manifest](t: Exp[(_, _, _, _, _, _, _, _, A, _, _, _, _, _, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple16(_, _, _, _, _, _, _, _, a, _, _, _, _, _, _, _)) ⇒ a
    case _ ⇒ Tuple16Access9(t)
  }
  def tuple16_get10[A: Manifest](t: Exp[(_, _, _, _, _, _, _, _, _, A, _, _, _, _, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple16(_, _, _, _, _, _, _, _, _, a, _, _, _, _, _, _)) ⇒ a
    case _ ⇒ Tuple16Access10(t)
  }
  def tuple16_get11[A: Manifest](t: Exp[(_, _, _, _, _, _, _, _, _, _, A, _, _, _, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple16(_, _, _, _, _, _, _, _, _, _, a, _, _, _, _, _)) ⇒ a
    case _ ⇒ Tuple16Access11(t)
  }
  def tuple16_get12[A: Manifest](t: Exp[(_, _, _, _, _, _, _, _, _, _, _, A, _, _, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple16(_, _, _, _, _, _, _, _, _, _, _, a, _, _, _, _)) ⇒ a
    case _ ⇒ Tuple16Access12(t)
  }
  def tuple16_get13[A: Manifest](t: Exp[(_, _, _, _, _, _, _, _, _, _, _, _, A, _, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple16(_, _, _, _, _, _, _, _, _, _, _, _, a, _, _, _)) ⇒ a
    case _ ⇒ Tuple16Access13(t)
  }
  def tuple16_get14[A: Manifest](t: Exp[(_, _, _, _, _, _, _, _, _, _, _, _, _, A, _, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple16(_, _, _, _, _, _, _, _, _, _, _, _, _, a, _, _)) ⇒ a
    case _ ⇒ Tuple16Access14(t)
  }
  def tuple16_get15[A: Manifest](t: Exp[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, A, _)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple16(_, _, _, _, _, _, _, _, _, _, _, _, _, _, a, _)) ⇒ a
    case _ ⇒ Tuple16Access15(t)
  }
  def tuple16_get16[A: Manifest](t: Exp[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A)])(
    implicit pos: SourceContext) = t match {
    case Def(ETuple16(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, a)) ⇒ a
    case _ ⇒ Tuple16Access16(t)
  }

  override def mirror[A: Manifest](e: Def[A], f: Transformer)(
    implicit pos: SourceContext): Exp[A] = (e match {
    case e @ ETuple9(a1, a2, a3, a4, a5, a6, a7, a8, a9) ⇒ make_tuple9(
      f(a1), f(a2), f(a3), f(a4), f(a5), f(a6), f(a7), f(a8), f(a9))(
        e.m1, e.m2, e.m3, e.m4, e.m5, e.m6, e.m7, e.m8, e.m9, pos)
    case e @ Tuple9Access1(t) ⇒ tuple9_get1(f(t))(mtype(e.m), pos)
    case e @ Tuple9Access2(t) ⇒ tuple9_get2(f(t))(mtype(e.m), pos)
    case e @ Tuple9Access3(t) ⇒ tuple9_get3(f(t))(mtype(e.m), pos)
    case e @ Tuple9Access4(t) ⇒ tuple9_get4(f(t))(mtype(e.m), pos)
    case e @ Tuple9Access5(t) ⇒ tuple9_get5(f(t))(mtype(e.m), pos)
    case e @ Tuple9Access6(t) ⇒ tuple9_get6(f(t))(mtype(e.m), pos)
    case e @ Tuple9Access7(t) ⇒ tuple9_get7(f(t))(mtype(e.m), pos)
    case e @ Tuple9Access8(t) ⇒ tuple9_get8(f(t))(mtype(e.m), pos)
    case e @ Tuple9Access9(t) ⇒ tuple9_get9(f(t))(mtype(e.m), pos)
    case Reflect(e @ Tuple9Access1(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple9Access1(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple9Access2(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple9Access2(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple9Access3(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple9Access3(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple9Access4(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple9Access4(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple9Access5(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple9Access5(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple9Access6(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple9Access6(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple9Access7(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple9Access7(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple9Access8(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple9Access8(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple9Access9(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple9Access9(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))

    case e @ ETuple16(
      a1, a2, a3, a4, a5, a6, a7, a8,
      a9, a10, a11, a12, a13, a14, a15, a16) ⇒ make_tuple16(
      f(a1), f(a2), f(a3), f(a4), f(a5), f(a6), f(a7), f(a8),
      f(a9), f(a10), f(a11), f(a12), f(a13), f(a14), f(a15), f(a16))(
        e.m1, e.m2, e.m3, e.m4, e.m5, e.m6, e.m7, e.m8,
        e.m9, e.m10, e.m11, e.m12, e.m13, e.m14, e.m15, e.m16, pos)
    case e @ Tuple16Access1(t)  ⇒ tuple16_get1(f(t))(mtype(e.m), pos)
    case e @ Tuple16Access2(t)  ⇒ tuple16_get2(f(t))(mtype(e.m), pos)
    case e @ Tuple16Access3(t)  ⇒ tuple16_get3(f(t))(mtype(e.m), pos)
    case e @ Tuple16Access4(t)  ⇒ tuple16_get4(f(t))(mtype(e.m), pos)
    case e @ Tuple16Access5(t)  ⇒ tuple16_get5(f(t))(mtype(e.m), pos)
    case e @ Tuple16Access6(t)  ⇒ tuple16_get6(f(t))(mtype(e.m), pos)
    case e @ Tuple16Access7(t)  ⇒ tuple16_get7(f(t))(mtype(e.m), pos)
    case e @ Tuple16Access8(t)  ⇒ tuple16_get8(f(t))(mtype(e.m), pos)
    case e @ Tuple16Access9(t)  ⇒ tuple16_get9(f(t))(mtype(e.m), pos)
    case e @ Tuple16Access10(t) ⇒ tuple16_get10(f(t))(mtype(e.m), pos)
    case e @ Tuple16Access11(t) ⇒ tuple16_get11(f(t))(mtype(e.m), pos)
    case e @ Tuple16Access12(t) ⇒ tuple16_get12(f(t))(mtype(e.m), pos)
    case e @ Tuple16Access13(t) ⇒ tuple16_get13(f(t))(mtype(e.m), pos)
    case e @ Tuple16Access14(t) ⇒ tuple16_get14(f(t))(mtype(e.m), pos)
    case e @ Tuple16Access15(t) ⇒ tuple16_get15(f(t))(mtype(e.m), pos)
    case e @ Tuple16Access16(t) ⇒ tuple16_get16(f(t))(mtype(e.m), pos)
    case Reflect(e @ Tuple16Access1(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple16Access1(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple16Access2(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple16Access2(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple16Access3(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple16Access3(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple16Access4(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple16Access4(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple16Access5(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple16Access5(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple16Access6(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple16Access6(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple16Access7(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple16Access7(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple16Access8(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple16Access8(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple16Access9(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple16Access9(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple16Access10(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple16Access10(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple16Access11(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple16Access11(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple16Access12(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple16Access12(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple16Access13(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple16Access13(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple16Access14(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple16Access14(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple16Access15(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple16Access15(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(e @ Tuple16Access16(t), u, es) ⇒ reflectMirrored(
      Reflect(Tuple16Access16(f(t))(mtype(e.m)), mapOver(f, u), f(es)))(mtype(manifest[A]))

    case _ ⇒ super.mirror(e, f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenMoreTupleOps extends ScalaGenTupleOps {
  val IR: MoreTupleOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ETuple9(a1, a2, a3, a4, a5, a6, a7, a8, a9) ⇒
      emitValDef(sym, "(" +
        quote(a1) + ", " + quote(a2) + ", " +
        quote(a3) + ", " + quote(a4) + ", " +
        quote(a5) + ", " + quote(a6) + ", " +
        quote(a7) + ", " + quote(a8) + ", " +
        quote(a9) + ")")
    case Tuple9Access1(t) ⇒ emitValDef(sym, quote(t) + "._1")
    case Tuple9Access2(t) ⇒ emitValDef(sym, quote(t) + "._2")
    case Tuple9Access3(t) ⇒ emitValDef(sym, quote(t) + "._3")
    case Tuple9Access4(t) ⇒ emitValDef(sym, quote(t) + "._4")
    case Tuple9Access5(t) ⇒ emitValDef(sym, quote(t) + "._5")
    case Tuple9Access6(t) ⇒ emitValDef(sym, quote(t) + "._6")
    case Tuple9Access7(t) ⇒ emitValDef(sym, quote(t) + "._7")
    case Tuple9Access8(t) ⇒ emitValDef(sym, quote(t) + "._8")
    case Tuple9Access9(t) ⇒ emitValDef(sym, quote(t) + "._9")

    case ETuple16(
      a1, a2, a3, a4, a5, a6, a7, a8,
      a9, a10, a11, a12, a13, a14, a15, a16) ⇒
      emitValDef(sym, "(" +
        quote(a1) + ", " + quote(a2) + ", " +
        quote(a3) + ", " + quote(a4) + ", " +
        quote(a5) + ", " + quote(a6) + ", " +
        quote(a7) + ", " + quote(a8) + ", " +
        quote(a9) + ", " + quote(a10) + ", " +
        quote(a11) + ", " + quote(a12) + ", " +
        quote(a13) + ", " + quote(a14) + ", " +
        quote(a15) + ", " + quote(a16) + ")")
    case Tuple16Access1(t)  ⇒ emitValDef(sym, quote(t) + "._1")
    case Tuple16Access2(t)  ⇒ emitValDef(sym, quote(t) + "._2")
    case Tuple16Access3(t)  ⇒ emitValDef(sym, quote(t) + "._3")
    case Tuple16Access4(t)  ⇒ emitValDef(sym, quote(t) + "._4")
    case Tuple16Access5(t)  ⇒ emitValDef(sym, quote(t) + "._5")
    case Tuple16Access6(t)  ⇒ emitValDef(sym, quote(t) + "._6")
    case Tuple16Access7(t)  ⇒ emitValDef(sym, quote(t) + "._7")
    case Tuple16Access8(t)  ⇒ emitValDef(sym, quote(t) + "._8")
    case Tuple16Access9(t)  ⇒ emitValDef(sym, quote(t) + "._9")
    case Tuple16Access10(t) ⇒ emitValDef(sym, quote(t) + "._10")
    case Tuple16Access11(t) ⇒ emitValDef(sym, quote(t) + "._11")
    case Tuple16Access12(t) ⇒ emitValDef(sym, quote(t) + "._12")
    case Tuple16Access13(t) ⇒ emitValDef(sym, quote(t) + "._13")
    case Tuple16Access14(t) ⇒ emitValDef(sym, quote(t) + "._14")
    case Tuple16Access15(t) ⇒ emitValDef(sym, quote(t) + "._15")
    case Tuple16Access16(t) ⇒ emitValDef(sym, quote(t) + "._16")

    case _                  ⇒ super.emitNode(sym, rhs)
  }
}
