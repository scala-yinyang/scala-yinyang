package ch.epfl.yinyang
package transformers

import ch.epfl.yinyang._
import ch.epfl.yinyang.transformers._
import scala.reflect.macros.blackbox.Context
import language.experimental.macros
import scala.collection.mutable

/**
 * Converts Scala features that can not be overridden to method calls that can be given
 * arbitrary semantics.
 *
 * ==Features covered are==
 * {{{
 *   var x = e              =>       var x = __newVar(e)
 *   if(c) t else e         =>       __ifThenElse(c, t, e)
 *   return t               =>       __return(t)
 *   x = t                  =>       __assign(x, t)
 *   while(c) b             =>       __whileDo(c, b)
 *   do b while c           =>       __doWhile(c, b)
 * }}}
 *
 * ===Poor man's infix methods for `Any` methods===
 * {{{
 *   t == t1                =>       infix_==(t, t1)
 *   t != t1                =>       infix_!=(t, t1)
 *   t.##                   =>       infix_##(t, t1)
 *   t.equals t1            =>       infix_equals(t, t1)
 *   t.hashCode             =>       infix_hashCode(t)
 *   t.asInstanceOf[T]      =>       infix_asInstanceOf[T](t)
 *   t.isInstanceOf[T]      =>       infix_isInstanceOf[T](t)
 *   t.toString             =>       infix_toString(t)
 * }}}
 *
 * ===Poor man's infix methods for `AnyRef` methods===
 * {{{
 *   t eq t1                =>       infix_eq(t, t1)
 *   t ne t1                =>       infix_ne(t, t1)
 *   t.notify               =>       infix_notify(t)
 *   t.notifyAll            =>       infix_notifyAll(t)
 *   t.synchronized[T](t1)  =>       infix_synchronized(t, t1)
 *   t.wait                 =>       infix_wait(t)
 *   t.wait(l)              =>       infix_wait(t, l)
 *   t.wait(t1, l)          =>       infix_wait(t, t1, l)
 * }}}
 *
 * ===Configurable===
 * {{{
 *   x => e                 =>       __lambda(x => e)
 *   f.apply(x)             =>       __app(f).apply(x)    // if `f` is a function object
 * }}}
 *
 * @todo
 * {{{
 *   try b catch c          =>       __tryCatch(b, c, f)
 *   throw e                =>       __throw(e)
 *   case class C { ... }   =>       ???
 *   Nothing                =>       ???
 *   Null                   =>       ???
 * }}}
 */
trait LanguageVirtualization extends MacroModule with TransformationUtils with DataDefs {
  import c.universe._

  val virtualizeLambda: Boolean = false
  val virtualizeApply: Boolean = false

  def virtualize(t: Tree): (Tree, Seq[DSLFeature]) = VirtualizationTransformer(t)

  object VirtualizationTransformer {
    def apply(tree: Tree) = {
      val t = new VirtualizationTransformer().apply(tree)
      log("(virtualized, Seq[Features]): " + t, 2)
      t
    }
  }

  private class VirtualizationTransformer extends Transformer {
    val lifted = mutable.ArrayBuffer[DSLFeature]()

    def liftFeature(receiver: Option[Tree], nme: String, args: List[Tree], targs: List[Tree] = Nil, trans: Tree => Tree = transform): Tree = {
      lifted += DSLFeature(receiver.map(_.tpe), nme, targs, List(args.map(_.tpe)))
      log(show(method(receiver.map(trans), nme, List(args.map(trans)), targs)), 3)
      method(receiver.map(trans), nme, List(args.map(trans)), targs)
    }

    override def transform(tree: Tree): Tree = {
      tree match {
        case x @ UnstageBlock(_) => x
        // sstucki: It seems necessary to keep the MUTABLE flag in the
        // new ValDef set, otherwise it becomes tricky to
        // "un-virtualize" a variable definition, if necessary
        // (e.g. if the DSL does not handle variable definitions in a
        // special way).
        case ValDef(mods, sym, tpt, rhs) if mods.hasFlag(Flag.MUTABLE) =>
          ValDef(mods, sym, tpt, liftFeature(None, "__newVar", List(rhs)))
        // Amir: fixes the problem with lifting types of ValDefs
        case ValDef(mods, sym, tpt, rhs) =>
          ValDef(mods, sym, tpt, transform(rhs))

        case f @ Function(vparams, body) if virtualizeLambda =>
          liftFeature(None, "__lambda", List(Function(vparams, transform(body))), Nil, x => x)

        case FunctionApply(qualifier, args) if virtualizeApply => {
          Apply(Select(liftFeature(None, "__app", List(qualifier)), TermName("apply")), args)
        }

        case t @ If(cond, thenBr, elseBr) =>
          liftFeature(None, "__ifThenElse", List(cond, thenBr, elseBr))

        case Return(e) =>
          liftFeature(None, "__return", List(e))

        case Assign(lhs, rhs) =>
          liftFeature(None, "__assign", List(lhs, transform(rhs)), Nil, x => x)

        case LabelDef(sym, List(), If(cond, Block(body :: Nil, Apply(Ident(label),
          List())), Literal(Constant(())))) if label == sym => // while(){}
          liftFeature(None, "__whileDo", List(cond, body))

        case LabelDef(sym, List(), Block(body :: Nil, If(cond, Apply(Ident(label),
          List()), Literal(Constant(()))))) if label == sym => // do while(){}
          liftFeature(None, "__doWhile", List(cond, body))

        case Apply(Select(qualifier, TermName("$eq$eq")), List(arg)) =>
          liftFeature(None, "infix_$eq$eq", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("$bang$eq")), List(arg)) =>
          liftFeature(None, "infix_$bang$eq", List(qualifier, arg))

        case Apply(lhs @ Select(qualifier, TermName("$hash$hash")), List()) =>
          liftFeature(None, "infix_$hash$hash", List(qualifier))

        case Apply(lhs @ Select(qualifier, TermName("equals")), List(arg)) =>
          liftFeature(None, "infix_equals", List(qualifier, arg))

        case Apply(lhs @ Select(qualifier, TermName("hashCode")), List()) =>
          liftFeature(None, "infix_hashCode", List(qualifier))

        case TypeApply(Select(qualifier, TermName("asInstanceOf")), targs) =>
          liftFeature(None, "infix_asInstanceOf", List(qualifier), targs)

        case TypeApply(Select(qualifier, TermName("isInstanceOf")), targs) =>
          liftFeature(None, "infix_isInstanceOf", List(qualifier), targs)

        case Apply(lhs @ Select(qualifier, TermName("toString")), List()) =>
          liftFeature(None, "infix_toString", List(qualifier))

        case Apply(lhs @ Select(qualifier, TermName("eq")), List(arg)) =>
          liftFeature(None, "infix_eq", List(qualifier, arg))

        case Apply(lhs @ Select(qualifier, TermName("ne")), List(arg)) =>
          liftFeature(None, "infix_ne", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("notify")), List()) =>
          liftFeature(None, "infix_notify", List(qualifier))

        case Apply(Select(qualifier, TermName("notifyAll")), List()) =>
          liftFeature(None, "infix_notifyAll", List(qualifier))

        case Apply(Select(qualifier, TermName("synchronized")), List(arg)) =>
          liftFeature(None, "infix_synchronized", List(qualifier, arg))

        case Apply(TypeApply(Select(qualifier, TermName("synchronized")), targs), List(arg)) =>
          liftFeature(None, "infix_synchronized", List(qualifier, arg), targs)

        case Apply(Select(qualifier, TermName("wait")), List()) =>
          liftFeature(None, "infix_wait", List(qualifier))

        case Apply(Select(qualifier, TermName("wait")), List(arg)
          ) if arg.tpe =:= typeOf[Long] =>
          liftFeature(None, "infix_wait", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("wait")), List(arg0, arg1)
          ) if arg0.tpe =:= typeOf[Long] && arg1.tpe =:= typeOf[Int] =>
          liftFeature(None, "infix_wait", List(qualifier, arg0, arg1))

        case Try(block, catches, finalizer) => {
          c.warning(tree.pos, "virtualization of try/catch expressions is not supported.")
          super.transform(tree)
        }

        case Throw(expr) => {
          c.warning(tree.pos, "virtualization of throw expressions is not supported.")
          super.transform(tree)
        }

        case Ident(x) if tree.symbol.isTerm && tree.symbol.asTerm.isVar => {
          liftFeature(None, "readVar", List(tree), Nil, x => x)
        }

        case ClassDef(mods, n, _, _) if mods.hasFlag(Flag.CASE) =>
          // sstucki: there are issues with the ordering of
          // virtualization and expansion of case classes (i.e. some
          // of the expanded code might be virtualized even though it
          // should not be and vice-versa).  So until we have decided
          // how proper virtualization of case classes should be done,
          // any attempt to do so should fail.
          println(tree)
          c.abort(tree.pos, "virtualization of case classes is not supported.")

        case _ =>
          super.transform(tree)
      }
    }

    object FunctionApply {
      val functionTypes = List(typeOf[() => _], typeOf[(_ => _)], typeOf[(_, _) => _])
      def isForFunctions(methodSymbol: Symbol): Boolean = {
        functionTypes.exists(_.typeSymbol == methodSymbol.owner)
      }

      def unapply(tree: Tree): Option[(Tree, List[Tree])] = tree match {
        case Apply(m @ Select(qualifier, TermName("apply")), args) if isForFunctions(m.symbol) => {
          Some(qualifier, args)
        }
        case _ => None
      }
    }

    def apply(tree: c.universe.Tree): (Tree, Seq[DSLFeature]) =
      (transform(tree), lifted.toSeq)
  }

}
