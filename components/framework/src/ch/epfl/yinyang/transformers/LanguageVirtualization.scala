package ch.epfl.yinyang
package transformers

import ch.epfl.yinyang._
import ch.epfl.yinyang.transformers._
import scala.reflect.macros.Context
import language.experimental.macros
import scala.collection.mutable

/**
 * Converts Scala features that can not be overriden to method calls that can be given
 * arbitrary semantics.
 *
 * Features covered are:
 *   if(c) t else e     =>       __ifThenElse(c, t, e)
 *   return t           =>       __return(t)
 *   x = t              =>       __assign(x, t)
 *   while(c) b         =>       __whileDo(c, b)
 *   do b while c       =>       __doWhile(c, b)
 *   t == t1            =>       t __== t1
 *   t != t1            =>       t __!= t1
 *   t eq t1            =>       t __eq t1
 *   t ne t1            =>       t __ne t1
 *   t.hashCode         =>       t.__hashCode
 *   t.##               =>       t.__##
 *   t.isInstanceOf[T]  =>       t.__isInstanceOf[T]
 *   t.asInstanceOf[T]  =>       t.__asInstanceOf[T]
 *   t.notify           =>       t.__notify
 *   t.notifyAll        =>       t.__notifyAll
 *   t.wait             =>       t.__wait()
 *   t.wait(l)          =>       t.__wait(l)
 *   t.wait(t1,l)       =>       t.__wait(t1, l)
 */
trait LanguageVirtualization extends MacroModule with TransformationUtils with DataDefs {
  import c.universe._

  def virtualize(t: Tree): (Tree, Seq[DSLFeature]) = VirtualizationTransformer(t)

  object VirtualizationTransformer {
    def apply(tree: Tree) = new VirtualizationTransformer().apply(tree)
  }

  object TermName { // TODO remove with 2.11
    def unapply(t: TermName): Option[String] = Some(t.toString)
  }

  private class VirtualizationTransformer extends Transformer {
    val lifted = mutable.ArrayBuffer[DSLFeature]()
    def liftFeature(receiver: Option[Tree], nme: String, args: List[List[Tree]], targs: List[Tree] = Nil): Tree = {
      lifted += DSLFeature(receiver.map(_.tpe), nme, targs, args.map(_.map(_.tpe)))
      log(show(method(receiver.map(transform), nme, args.map(_.map(transform)), targs)))
      method(receiver.map(transform), nme, args.map(_.map(transform)), targs)
    }
    override def transform(tree: Tree): Tree = {
      tree match {
        case ValDef(mods, sym, tpt, rhs) if mods.hasFlag(Flag.MUTABLE) =>
          ValDef(mods, sym, tpt, liftFeature(None, "__newVar", List(List(rhs))))

        case t @ If(cond, then, elze) =>
          liftFeature(None, "__ifThenElse", List(List(transform(cond), transform(then), transform(elze))))

        case Return(e) =>
          liftFeature(None, "__return", List(List(transform(e))))

        case Assign(lhs, rhs) =>
          liftFeature(None, "__assign", List(List(transform(lhs), transform(rhs))))

        case LabelDef(sym, List(), If(cond, Block(body :: Nil, Apply(Ident(label),
          List())), Literal(Constant()))) if label == sym => // while(){}
          liftFeature(None, "__whileDo", List(List(transform(cond), transform(body))))

        case LabelDef(sym, List(), Block(body :: Nil, If(cond, Apply(Ident(label),
          List()), Literal(Constant())))) if label == sym => // do while(){}
          liftFeature(None, "__doWhile", List(List(transform(cond), transform(body))))

        case Apply(Select(qualifier, TermName("$eq$eq")), List(arg)) =>
          liftFeature(Some(transform(qualifier)), "__$eq$eq", List(List(transform(arg))))

        case Apply(Select(qualifier, TermName("$bang$eq")), List(arg)) =>
          liftFeature(Some(transform(qualifier)), "__$bang$eq", List(List(transform(arg))))

        case Apply(lhs @ Select(qualifier, TermName("eq")), List(arg)) =>
          liftFeature(Some(transform(qualifier)), "__eq", List(List(transform(arg))))

        case Apply(lhs @ Select(qualifier, TermName("ne")), List(arg)) =>
          liftFeature(Some(transform(qualifier)), "__ne", List(List(transform(arg))))

        case Apply(lhs @ Select(qualifier, TermName("hashCode")), List()) =>
          liftFeature(Some(transform(qualifier)), "__hashCode", List(List()))

        case Apply(lhs @ Select(qualifier, TermName("$hash$hash")), List()) =>
          liftFeature(Some(transform(qualifier)), "__$hash$hash", List(List()))

        case TypeApply(Select(qualifier, TermName("asInstanceOf")), targs) =>
          liftFeature(Some(transform(qualifier)), "__asInstanceOf", List(List()), targs)

        case TypeApply(Select(qualifier, TermName("isInstanceOf")), targs) =>
          liftFeature(Some(transform(qualifier)), "__isInstanceOf", List(List()), targs)

        case Apply(Select(qualifier, TermName("notify")), List()) =>
          liftFeature(Some(transform(qualifier)), "__notify", List(List()))

        case Apply(Select(qualifier, TermName("notifyAll")), List()) =>
          liftFeature(Some(transform(qualifier)), "__notifyAll", List())

        case Apply(Select(qualifier, TermName("wait")), List()) =>
          liftFeature(Some(transform(qualifier)), "__wait", List())

        case Apply(Select(qualifier, TermName("wait")), List(arg)
          ) if arg.tpe =:= typeOf[Long] =>
          liftFeature(Some(transform(qualifier)), "__wait", List(List(arg)))

        case Apply(Select(qualifier, TermName("wait")), List(arg0, arg1)
          ) if arg0.tpe =:= typeOf[Long] && arg1.tpe =:= typeOf[Int] =>
          liftFeature(Some(transform(qualifier)), "__wait", List(List(arg0, arg1)))

        case _ =>
          super.transform(tree)
      }
    }
    def apply(tree: c.universe.Tree): (Tree, Seq[DSLFeature]) =
      (transform(tree), lifted.toSeq)
  }

}