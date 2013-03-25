package collections.lms

import java.io.PrintWriter

import scala.collection.mutable.{ HashMap /*, Set*/ }

import scala.language.implicitConversions

import scala.reflect.SourceContext

import scala.virtualization.lms.common.{
  HashMapOps,
  HashMapOpsExp,
  BaseGenHashMapOps,
  ScalaGenHashMapOps
}

trait MoreHashMapOps extends HashMapOps {

  implicit def repHashMapToMoreHashMapOps[K: Manifest, V: Manifest](
    m: Rep[HashMap[K, V]]) = new MoreHashMapOpsCls(m)

  class MoreHashMapOpsCls[K: Manifest, V: Manifest](m: Rep[HashMap[K, V]])
    extends hashmapOpsCls[K, V](m) {
    def getOrElse(key: Rep[K], default: ⇒ Rep[V]) =
      hashmap_getOrElse(key, () ⇒ default)(m)
    def filter(f: Rep[(K, V)] ⇒ Rep[Boolean]) = hashmap_filter(f)(m)
    def flatMap[K2: Manifest, V2: Manifest](f: Rep[(K, V)] ⇒ Rep[List[(K2, V2)]]) =
      hashmap_flatMap(f)(m)
    def map[K2: Manifest, V2: Manifest](f: Rep[(K, V)] ⇒ Rep[(K2, V2)]) =
      hashmap_map(f)(m)
    def toList = hashmap_toList(m)
    //def withDefaultValue[K: Manifest, V: Manifest](d: Rep[V]) =
    //  hashMap_withDefaultValue(d)(m)
  }

  def hashmap_getOrElse[K: Manifest, V: Manifest](
    k: Rep[K], d: () ⇒ Rep[V])(m: Rep[HashMap[K, V]])(
      implicit pos: SourceContext): Rep[V]
  def hashmap_filter[K: Manifest, V: Manifest](f: Rep[(K, V)] ⇒ Rep[Boolean])(
    m: Rep[HashMap[K, V]])(implicit pos: SourceContext): Rep[HashMap[K, V]]
  def hashmap_flatMap[K: Manifest, V: Manifest, K2: Manifest, V2: Manifest](
    f: Rep[(K, V)] ⇒ Rep[List[(K2, V2)]])(m: Rep[HashMap[K, V]])(
      implicit pos: SourceContext): Rep[HashMap[K2, V2]]
  def hashmap_map[K: Manifest, V: Manifest, K2: Manifest, V2: Manifest](
    f: Rep[(K, V)] ⇒ Rep[(K2, V2)])(m: Rep[HashMap[K, V]])(
      implicit pos: SourceContext): Rep[HashMap[K2, V2]]
  def hashmap_toList[K: Manifest, V: Manifest](m: Rep[HashMap[K, V]])(
    implicit pos: SourceContext): Rep[List[(K, V)]]
  //def hashMap_withDefaultValue[K: Manifest, V: Manifest](d: Rep[V])(
  //  m: Rep[HashMap[K, V]])(implicit pos: SourceContext): Rep[HashMap[K, V]]
}

trait MoreHashMapOpsExp extends HashMapOpsExp with MoreHashMapOps {

  case class HashMapGetOrElse[K: Manifest, V: Manifest](
    m: Exp[HashMap[K, V]], k: Exp[K], d: Block[V]) extends HashMapDef[K, V, V]
  case class HashMapFilter[K: Manifest, V: Manifest](
    m: Exp[HashMap[K, V]], x: Sym[(K, V)], body: Block[Boolean])
    extends HashMapDef[K, V, HashMap[K, V]]
  case class HashMapFlatMap[K: Manifest, V: Manifest, K2: Manifest, V2: Manifest](
    m: Exp[HashMap[K, V]], x: Sym[(K, V)], body: Block[List[(K2, V2)]])
    extends HashMapDef[K, V, HashMap[K2, V2]] {
    val mK2 = manifest[K2]
    val mV2 = manifest[V2]
  }
  case class HashMapMap[K: Manifest, V: Manifest, K2: Manifest, V2: Manifest](
    m: Exp[HashMap[K, V]], x: Sym[(K, V)], body: Block[(K2, V2)])
    extends HashMapDef[K, V, HashMap[K2, V2]] {
    val mK2 = manifest[K2]
    val mV2 = manifest[V2]
  }
  case class HashMapToList[K: Manifest, V: Manifest](m: Exp[HashMap[K, V]])
    extends HashMapDef[K, V, List[(K, V)]]
  //case class HashMapWithDefaultValue[K: Manifest, V: Manifest](
  //  m: Exp[HashMap[K, V]], d: Rep[V])
  //    extends HashMapDef[K, V, HashMap[K, V]]

  def hashmap_getOrElse[K: Manifest, V: Manifest](
    k: Exp[K], d: () ⇒ Exp[V])(m: Exp[HashMap[K, V]])(
      implicit pos: SourceContext) = {
    val d2 = reifyEffects(d())
    reflectEffect(HashMapGetOrElse(m, k, d2), summarizeEffects(d2).star)
  }
  def hashmap_filter[K: Manifest, V: Manifest](f: Exp[(K, V)] ⇒ Exp[Boolean])(
    m: Exp[HashMap[K, V]])(implicit pos: SourceContext) = {
    val x = fresh[(K, V)]
    val body = reifyEffects(f(x))
    reflectEffect(HashMapFilter(m, x, body), summarizeEffects(body).star)
  }
  def hashmap_flatMap[K: Manifest, V: Manifest, K2: Manifest, V2: Manifest](
    f: Exp[(K, V)] ⇒ Exp[List[(K2, V2)]])(m: Exp[HashMap[K, V]])(
      implicit pos: SourceContext) = {
    val x = fresh[(K, V)]
    val body = reifyEffects(f(x))
    reflectEffect(HashMapFlatMap(m, x, body), summarizeEffects(body).star)
  }
  def hashmap_map[K: Manifest, V: Manifest, K2: Manifest, V2: Manifest](
    f: Exp[(K, V)] ⇒ Exp[(K2, V2)])(m: Exp[HashMap[K, V]])(
      implicit pos: SourceContext) = {
    val x = fresh[(K, V)]
    val body = reifyEffects(f(x))
    reflectEffect(HashMapMap(m, x, body), summarizeEffects(body).star)
  }
  def hashmap_toList[K: Manifest, V: Manifest](m: Exp[HashMap[K, V]])(
    implicit pos: SourceContext) = HashMapToList(m)
  //def hashMap_withDefaultValue[K: Manifest, V: Manifest](d: Exp[V])(
  //  m: Exp[HashMap[K, V]])(implicit pos: SourceContext): Def[List[(K, V)]] =
  //  HashMapToList(m, d)

  override def mirror[A: Manifest](e: Def[A], f: Transformer)(
    implicit pos: SourceContext): Exp[A] = {
    /* ergh Sorry
      (e match { // FIXME:
      // case e @ HashMapGetOrElse(m, k, d) =>
      //   hashmap_getOrElse(f(k), f(d))(f(m))(e.mK, e.mV, pos)
      // case e @ HashMapFilter(m, x, body) =>
      //   hashmap_filter(f(x).asInstanceof[Sym[_]], f(body))(f(m))(e.mK, e.mV, pos)
      // case e @ HashMapFlatMap(m, x, body) =>
      //   hashmap_flatMap(f(x).asInstanceof[Sym[_]], f(body))(f(m))(e.mK, e.mV, pos)
      // case e @ HashMapMap(m, x, body) =>
      //   hashmap_map(f(x).asInstanceof[Sym[_]], f(body))(f(m))(e.mK, e.mV, pos)
      case e @ HashMapToList(m) ⇒
        hashmap_toList(f(m))(e.mK, e.mV, pos)
      //case e @ HashMapWithDefaultValue(m, d) =>
      //  hashmap_withDefaultValue(f(m), f(d))(e.mK, e.mV, pos)
      //case Reflect(e @ HashMapGetOrElse(m, k, d), u, es) =>
      //  reflectMirrored(Reflect(HashMapGetOrElse(
      //    f(m), f(k), f(d))(e.mK, e.mV), mapOver(f, u), f(es)))(mtype(manifest[A]))
      case Reflect(e @ HashMapFilter(m, x, body), u, es) ⇒
        reflectMirrored(Reflect(HashMapFilter(
          f(m), f(x).asInstanceOf[Sym[(_, _)]], f(body))(
            e.mK, e.mV), mapOver(f, u), f(es)))(mtype(manifest[A]))
      case Reflect(e @ HashMapFlatMap(m, x, body), u, es) ⇒
        reflectMirrored(Reflect(HashMapFlatMap(
          f(m), f(x).asInstanceOf[Sym[(_, _)]], f(body))(
            e.mK, e.mV, e.mK2, e.mV2), mapOver(f, u), f(es)))(mtype(manifest[A]))
      case Reflect(e @ HashMapMap(m, x, body), u, es) ⇒
        reflectMirrored(Reflect(HashMapMap(
          f(m), f(x).asInstanceOf[Sym[(_, _)]], f(body))(
            e.mK, e.mV, e.mK2, e.mV2), mapOver(f, u), f(es)))(mtype(manifest[A]))
      case Reflect(e @ HashMapToList(m), u, es) ⇒
        reflectMirrored(Reflect(HashMapToList(
          f(m))(e.mK, e.mV), mapOver(f, u), f(es)))(mtype(manifest[A]))
      //case Reflect(e @ HashMapWithDefaultValue(m, d), u, es) =>
      //  reflectMirrored(Reflect(HashMapWithDefaultValue(
      //    f(m), f(d))(e.mK, e.mV), mapOver(f, u), f(es)))(mtype(manifest[A]))
      case _ ⇒ super.mirror(e, f)
    }).asInstanceOf[Exp[A]] // why??
*/
    super.mirror(e, f)
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case HashMapGetOrElse(m, k, d)  ⇒ syms(m) ::: syms(k) ::: syms(d)
    case HashMapFilter(m, _, body)  ⇒ syms(m) ::: syms(body)
    case HashMapFlatMap(m, _, body) ⇒ syms(m) ::: syms(body)
    case HashMapMap(m, _, body)     ⇒ syms(m) ::: syms(body)
    case HashMapToList(m)           ⇒ syms(m)
    //case HashMapWithDefaultValue(m, d) => syms(m) ::: syms(d)
    case _                          ⇒ super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case HashMapGetOrElse(_, _, d)  ⇒ effectSyms(d)
    case HashMapFilter(_, x, body)  ⇒ x :: effectSyms(body)
    case HashMapFlatMap(_, x, body) ⇒ x :: effectSyms(body)
    case HashMapMap(_, x, body)     ⇒ x :: effectSyms(body)
    case HashMapToList(_)           ⇒ Nil
    //case HashMapWithDefaultValue(_, _) => Nil
    case _                          ⇒ super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case HashMapGetOrElse(m, k, d)  ⇒ freqNormal(m) ::: freqNormal(k) ::: freqNormal(d)
    case HashMapFilter(m, _, body)  ⇒ freqNormal(m) ::: freqHot(body)
    case HashMapFlatMap(m, _, body) ⇒ freqNormal(m) ::: freqHot(body)
    case HashMapMap(m, _, body)     ⇒ freqNormal(m) ::: freqHot(body)
    case HashMapToList(m)           ⇒ freqNormal(m)
    //case HashMapWithDefaultValue(m, d) => freqNormal(m) ::: freqNormal(d)
    case _                          ⇒ super.symsFreq(e)
  }
}

trait BaseGenMoreHashMapOps extends BaseGenHashMapOps {
  val IR: MoreHashMapOpsExp
  import IR._

}

trait ScalaGenMoreHashMapOps extends ScalaGenHashMapOps with BaseGenMoreHashMapOps {
  val IR: MoreHashMapOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case HashMapGetOrElse(m, k, d) ⇒
      emitValDef(sym, quote(m) + ".getOrElse(" + quote(k) + ", {")
      emitBlock(d)
      stream.println(quote(getBlockResult(d)))
      stream.println("})")
    case HashMapFilter(m, x, body) ⇒
      emitValDef(sym, quote(m) + ".filter{" + quote(x) + " => ")
      emitBlock(body)
      stream.println(quote(getBlockResult(body)))
      stream.println("}")
    case HashMapFlatMap(m, x, body) ⇒
      emitValDef(sym, quote(m) + ".flatMap{" + quote(x) + " => ")
      emitBlock(body)
      stream.println(quote(getBlockResult(body)))
      stream.println("}")
    case HashMapMap(m, x, body) ⇒
      emitValDef(sym, quote(m) + ".map{" + quote(x) + " => ")
      emitBlock(body)
      stream.println(quote(getBlockResult(body)))
      stream.println("}")
    case HashMapToList(m) ⇒ emitValDef(sym, quote(m) + ".toList")
    case _                ⇒ super.emitNode(sym, rhs)
  }
}
