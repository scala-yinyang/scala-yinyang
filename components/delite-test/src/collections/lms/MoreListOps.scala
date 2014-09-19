package collections.lms

import java.io.PrintWriter

import scala.collection.mutable.HashMap

import scala.language.implicitConversions

import scala.reflect.SourceContext

import scala.virtualization.lms.common.{
  ListOps,
  ListOpsExp,
  ListOpsExpOpt,
  BaseGenListOps,
  ScalaGenListOps
}

trait MoreListOps extends ListOps {

  implicit def varToMoreListOps[T: Manifest](x: Var[List[T]]) =
    new MoreListOpsCls(__readVar(x))
  implicit def repToMoreListOps[T: Manifest](a: Rep[List[T]]) =
    new MoreListOpsCls(a)
  implicit def listToListMoreOps[T: Manifest](a: List[T]) =
    new MoreListOpsCls(unit(a))

  class MoreListOpsCls[A: Manifest](xs: Rep[List[A]]) extends ListOpsCls[A](xs) {
    def drop(n: Rep[Int]): Rep[List[A]] = list_drop(n)(xs)
    def foldLeft[B: Manifest](z: Rep[B])(op: (Rep[B], Rep[A]) => Rep[B]): Rep[B] =
      list_foldLeft(z, op)(xs)
    def groupBy[B: Manifest](f: Rep[A] => Rep[B]): Rep[HashMap[B, List[A]]] =
      list_groupBy(f)(xs)
    def take(n: Rep[Int]): Rep[List[A]] = list_take(n)(xs)
    def length: Rep[Int] = list_length(xs)
  }

  def list_drop[A: Manifest](n: Rep[Int])(xs: Rep[List[A]]): Rep[List[A]]
  def list_groupBy[A: Manifest, B: Manifest](f: Rep[A] => Rep[B])(
    xs: Rep[List[A]]): Rep[HashMap[B, List[A]]]
  def list_foldLeft[A: Manifest, B: Manifest](
    z: Rep[B], op: (Rep[B], Rep[A]) => Rep[B])(xs: Rep[List[A]]): Rep[B]
  def list_take[A: Manifest](n: Rep[Int])(xs: Rep[List[A]]): Rep[List[A]]
  def list_length[A: Manifest](xs: Rep[List[A]]): Rep[Int]
}

trait MoreListOpsExp extends MoreListOps with ListOpsExp {
  case class ListDrop[A: Manifest](
    xs: Rep[List[A]], n: Rep[Int]) extends Def[List[A]]
  case class ListGroupBy[A: Manifest, B: Manifest](
    xs: Rep[List[A]], x: Sym[A], body: Block[B])
    extends Def[HashMap[B, List[A]]] {
    val mA = manifest[A]
    val mB = manifest[B]
  }
  case class ListFoldLeft[A: Manifest, B: Manifest](
    xs: Rep[List[A]], z: Rep[B], x1: Sym[B], x2: Sym[A], body: Block[B])
    extends Def[B]
  case class ListTake[A: Manifest](
    xs: Rep[List[A]], n: Rep[Int]) extends Def[List[A]]
  case class ListLength[A: Manifest](
    xs: Rep[List[A]]) extends Def[Int]

  def list_drop[A: Manifest](n: Rep[Int])(xs: Rep[List[A]]): Rep[List[A]] =
    ListDrop(xs, n)
  def list_groupBy[A: Manifest, B: Manifest](f: Rep[A] => Rep[B])(
    xs: Rep[List[A]]): Rep[HashMap[B, List[A]]] = {
    val x = fresh[A]
    val body = reifyEffects(f(x))
    reflectEffect(ListGroupBy(xs, x, body), summarizeEffects(body).star)
  }
  def list_foldLeft[A: Manifest, B: Manifest](
    z: Rep[B], op: (Rep[B], Rep[A]) => Rep[B])(xs: Rep[List[A]]): Rep[B] = {
    val x1 = fresh[B]
    val x2 = fresh[A]
    val body = reifyEffects(op(x1, x2))
    reflectEffect(ListFoldLeft(xs, z, x1, x2, body), summarizeEffects(body).star)
  }
  def list_take[A: Manifest](n: Rep[Int])(xs: Rep[List[A]]): Rep[List[A]] =
    ListTake(xs, n)
  def list_length[A: Manifest](xs: Rep[List[A]]): Rep[Int] =
    ListLength(xs)

  override def syms(e: Any): List[Sym[Any]] = e match {
    case ListDrop(xs, n)                 => syms(xs) ::: syms(n)
    case ListGroupBy(xs, _, body)        => syms(xs) ::: syms(body)
    case ListFoldLeft(xs, z, _, _, body) => syms(xs) ::: syms(z) ::: syms(body)
    case ListTake(xs, n)                 => syms(xs) ::: syms(n)
    case ListLength(xs)                  => syms(xs)
    case _                               => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ListDrop(_, _)                   => Nil
    case ListGroupBy(_, x, body)          => x :: effectSyms(body)
    case ListFoldLeft(_, _, x1, x2, body) => x1 :: x2 :: effectSyms(body)
    case ListTake(_, _)                   => Nil
    case ListLength(xs)                   => Nil
    case _                                => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ListDrop(xs, n)          => freqNormal(xs) ::: freqNormal(n)
    case ListGroupBy(xs, _, body) => freqNormal(xs) ::: freqHot(body)
    case ListFoldLeft(xs, z, _, _, body) =>
      freqNormal(xs) ::: freqNormal(z) ::: freqHot(body)
    case ListTake(xs, n) => freqNormal(xs) ::: freqNormal(n)
    case ListLength(xs)  => freqNormal(xs)
    case _               => super.symsFreq(e)
  }
}

trait MoreListOpsExpOpt extends MoreListOpsExp with ListOpsExpOpt

trait BaseGenMoreListOps extends BaseGenListOps {
  val IR: MoreListOpsExp
  import IR._

}

trait ScalaGenMoreListOps extends ScalaGenListOps {
  val IR: MoreListOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ListDrop(xs, n) =>
      emitValDef(sym, quote(xs) + ".drop(" + quote(n) + ")")
    case lgb @ ListGroupBy(xs, x, body) => {
      emitValDef(sym, "collection.mutable.HashMap[" + remap(lgb.mB) +
        ", List[" + remap(lgb.mA) + "]]() ++= " + quote(xs) + ".groupBy{ " +
        quote(x) + " => ")
      emitBlock(body)
      stream.println(quote(getBlockResult(body)))
      stream.println("}")
    }
    case ListFoldLeft(xs, z, x1, x2, body) => {
      emitValDef(sym, quote(xs) + ".foldLeft(" + quote(z) + "){ (" +
        quote(x1) + ", " + quote(x2) + ") => ")
      emitBlock(body)
      stream.println(quote(getBlockResult(body)))
      stream.println("}")
    }
    case ListTake(xs, n) =>
      emitValDef(sym, quote(xs) + ".take(" + quote(n) + ")")
    case ListLength(xs) =>
      emitValDef(sym, quote(xs) + ".length")
    case _ => super.emitNode(sym, rhs)
  }
}
