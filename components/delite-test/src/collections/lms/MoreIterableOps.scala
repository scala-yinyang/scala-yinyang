package collections.lms

import java.io.PrintWriter

import scala.collection.mutable.HashMap

import scala.language.implicitConversions

import scala.reflect.SourceContext

import scala.virtualization.lms.common.{
  IterableOps,
  IterableOpsExp,
  BaseGenIterableOps,
  ScalaGenIterableOps
}

trait MoreIterableOps extends IterableOps {

  implicit def varToMoreIterableOps[A: Manifest](x: Var[Iterable[A]]) =
    new MoreIterableOpsCls(__readVar(x))
  implicit def repIterableToMoreIterableOps[T: Manifest](a: Rep[Iterable[T]]) =
    new MoreIterableOpsCls(a)
  implicit def iterableToMoreIterableOps[T: Manifest](a: Iterable[T]) =
    new MoreIterableOpsCls(unit(a))

  class MoreIterableOpsCls[T: Manifest](xs: Rep[Iterable[T]])
    extends IterableOpsCls[T](xs) {
    def toList(implicit pos: SourceContext) = iterable_toList(xs)
  }

  def iterable_toList[T: Manifest](
    xs: Rep[Iterable[T]])(implicit pos: SourceContext): Rep[List[T]]
}

trait MoreIterableOpsExp extends MoreIterableOps with IterableOpsExp {

  case class IterableToList[T: Manifest](xs: Exp[Iterable[T]])
    extends Def[List[T]] {
    val m = manifest[T]
  }

  def iterable_toList[T: Manifest](xs: Exp[Iterable[T]])(
    implicit pos: SourceContext) = IterableToList(xs)

  override def mirror[A: Manifest](e: Def[A], f: Transformer)(
    implicit pos: SourceContext): Exp[A] = {
    (e match {
      case ita @ IterableToList(xs) => iterable_toList(f(xs))(ita.m, pos)
      case Reflect(ita @ IterableToList(xs), u, es) =>
        reflectMirrored(Reflect(IterableToList(f(xs))(ita.m), mapOver(f, u), f(es)))(mtype(manifest[A]))
      case _ => super.mirror(e, f)
    }).asInstanceOf[Exp[A]] // why??
  }
}

trait BaseGenMoreIterableOps extends BaseGenIterableOps {
  val IR: MoreIterableOpsExp
  import IR._

}

trait ScalaGenMoreIterableOps extends BaseGenMoreIterableOps with ScalaGenIterableOps {
  val IR: MoreIterableOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case IterableToList(xs) => emitValDef(sym, quote(xs) + ".toList")
    case _                  => super.emitNode(sym, rhs)
  }
}
