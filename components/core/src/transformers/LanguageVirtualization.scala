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
 * == Control Structures==
 * {{{
 *   if(c) t else e         =>       __ifThenElse(c, t, e)
 *   while(c) b             =>       __whileDo(c, b)
 *   do b while c           =>       __doWhile(c, b)
 *   return t               =>       __return(t)
 * }}}
 * ===Virtualization of `Any` methods===
 * {{{
 *   var x = e              =>       var x = __var(e)
 *   x = t                  =>       __assign(x, t)
 *   x // if x is a val     =>       __read(x)
 * }}}
 *
 * ===Virtualization of `Any` methods===
 * {{{
 *   t == t1                =>       infix_==(t, t1)
 *   t != t1                =>       infix_!=(t, t1)
 *   t.##                   =>       infix_##(t)
 *   t.asInstanceOf[T]      =>       infix_asInstanceOf[T](t)
 *   t.isInstanceOf[T]      =>       infix_isInstanceOf[T](t)
 *   t.getClass             =>       infix_getClass(t)
 *
 *   // configurable
 *   t.toString             =>       infix_toString(t)
 *   t.hashCode             =>       infix_hashCode(t)
 *   t.equals t1            =>       infix_equals(t, t1)
 * }}}
 *
 * ===Vritualization of `AnyRef` methods===
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
 *   val x = b              =>       __valDef(b)
 * }}}
 *
 * ===Forbiden===
 * {{{
 *     class C { ... }
 *     trait T
 *     object O
 * }}}
 *
 */
trait LanguageVirtualization extends MacroModule with TransformationUtils with DataDefs {
  import c.universe._

  val virtualizeFunctions: Boolean
  val failCompilation: Boolean
  val virtualizeVal: Boolean
  val nameBindings: Boolean = false
  val restrictDefinitions: Boolean = true
  val translateNonFinalUniversal: Boolean = true

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

    def named(name: TermName): Seq[Tree] = if (nameBindings) Seq(q"$name") else Seq()

    override def transform(tree: Tree): Tree = {
      tree match {
        case f @ Function(vparams, body) if virtualizeFunctions =>
          val tree = transform(body)
          liftFeature(None, "__lambda", List(Function(vparams, tree)), Nil, trans = x => x)

        case FunctionApply(qualifier, args, targs) if virtualizeFunctions =>
          Apply(Select(liftFeature(None, "__app", List(qualifier), targs), TermName("apply")), args map transform)

        case t @ If(cond, thenBr, elseBr) =>
          liftFeature(None, "__ifThenElse", List(cond, thenBr, elseBr))

        case Return(e) =>
          liftFeature(None, "__return", List(e))

        case LabelDef(sym, List(), If(cond, Block(body :: Nil, Apply(Ident(label),
          List())), Literal(Constant(())))) if label == sym => // while(){}
          liftFeature(None, "__whileDo", List(cond, body))

        case LabelDef(sym, List(), Block(body :: Nil, If(cond, Apply(Ident(label),
          List()), Literal(Constant(()))))) if label == sym => // do while(){}
          liftFeature(None, "__doWhile", List(cond, body))

        case Try(block, catches, finalizer) =>
          val arg = Ident(TermName("x"))
          val etaExpandedCatch = Function(
            List(ValDef(Modifiers(Flag.PARAM), TermName("x"), Ident(TypeName("Any")), EmptyTree)),
            if (catches == Nil) arg else Match(arg, catches))
          val finalizerTree = if (finalizer == EmptyTree) q"null" else finalizer
          val tparams = if (tree.tpe == null) Nil else List(TypeTree(tree.tpe))
          liftFeature(None, "__try", List(block, etaExpandedCatch, finalizerTree), tparams)

        case Throw(expr) =>
          liftFeature(None, "__throw", List(expr))

        // TODO multiple apply
        case Apply(Select(New(nme), termNames.CONSTRUCTOR), args) =>
          liftFeature(None, "__New_" + nme.toString, args)

        //
        // Variables virtualization
        //
        case ValDef(mods, sym, tpt, rhs) if mods.hasFlag(Flag.PARAM) =>
          ValDef(mods, sym, tpt, transform(rhs))

        // sstucki: It seems necessary to keep the MUTABLE flag in the
        // new ValDef set, otherwise it becomes tricky to
        // "un-virtualize" a variable definition, if necessary
        // (e.g. if the DSL does not handle variable definitions in a
        // special way).
        case ValDef(mods, sym, tpt, rhs) if mods.hasFlag(Flag.MUTABLE) =>
          ValDef(mods, sym, tpt, liftFeature(None, "__varDef", List(rhs) ++ named(sym)))

        case Assign(lhs, rhs) =>
          liftFeature(None, "__assign", List(lhs, transform(rhs)), Nil, x => x)

        case ValDef(mods, sym, tpt, rhs) if mods.hasFlag(Flag.LAZY) =>
          ValDef(mods, sym, tpt, liftFeature(None, "__lazyValDef", List(rhs) ++ named(sym)))

        case ValDef(mods, sym, tpt, rhs) =>
          val newRhs =
            if (virtualizeVal) liftFeature(None, "__valDef", List(rhs) ++ named(sym))
            else transform(rhs)
          ValDef(mods, sym, tpt, newRhs)

        case Ident(x) if (tree.symbol.isTerm && (
          tree.symbol.asTerm.isVar ||
          tree.symbol.asTerm.isLazy ||
          (tree.symbol.asTerm.isVal && virtualizeVal))) =>
          liftFeature(None, "__read", List(tree), Nil, x => x)

        //
        // Universal methods virtualization
        //
        case Apply(Select(qualifier, TermName("$eq$eq")), List(arg)) =>
          liftFeature(None, "infix_$eq$eq", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("$bang$eq")), List(arg)) =>
          liftFeature(None, "infix_$bang$eq", List(qualifier, arg))

        case Apply(lhs @ Select(qualifier, TermName("$hash$hash")), List()) =>
          liftFeature(None, "infix_$hash$hash", List(qualifier))

        case Apply(lhs @ Select(qualifier, TermName("equals")), List(arg)) if translateNonFinalUniversal =>
          liftFeature(None, "infix_equals", List(qualifier, arg))

        case Apply(lhs @ Select(qualifier, TermName("hashCode")), List()) if translateNonFinalUniversal =>
          liftFeature(None, "infix_hashCode", List(qualifier))

        case TypeApply(Select(qualifier, TermName("asInstanceOf")), targs) =>
          liftFeature(None, "infix_asInstanceOf", List(qualifier), targs)

        case TypeApply(Select(qualifier, TermName("isInstanceOf")), targs) =>
          liftFeature(None, "infix_isInstanceOf", List(qualifier), targs)

        case TypeApply(Select(qualifier, TermName("getClass"))) =>
          liftFeature(None, "infix_getClass", List(qualifier), Nil)

        case Apply(lhs @ Select(qualifier, TermName("toString")), List()) if translateNonFinalUniversal =>
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

        case Typed(x, Ident(typeNames.WILDCARD_STAR)) =>
          Typed(liftFeature(None, "__castVarArg", List(x)), Ident(typeNames.WILDCARD_STAR))

        //
        // Restrictions
        //
        // Case-classes are never supported: i) with @virtualize they badly interact with
        //  further case class expansion, and ii) in macro mode we do not support them.
        case ClassDef(mods, _, _, _) if mods.hasFlag(Flag.CASE) =>
          c.abort(tree.pos, "Virtualization of case classes is not supported.")

        case ClassDef(_, _, _, _) if restrictDefinitions =>
          c.abort(tree.pos, "Virtualization of classes and traits is not supported.")

        case ModuleDef(_, _, _) if restrictDefinitions =>
          c.abort(tree.pos, "Virtualization of modules is not supported.")

        case _ =>
          super.transform(tree)
      }
    }

    object FunctionApply {
      private val functionArity = 22
      private val functionSymbols = (0 to functionArity)
        .map(x => "scala.Function" + x)
        .map(c.mirror.staticClass)

      def isFunction(methodSymbol: Symbol): Boolean =
        functionSymbols.contains(methodSymbol.owner)

      def unapply(tree: Tree): Option[(Tree, List[Tree], List[Tree])] = tree match {
        case Apply(m @ Select(qualifier, TermName("apply")), args) if isFunction(m.symbol) =>
          val targs = m.tpe match {
            case null => Nil
            case MethodType(params, respte) => {
              val argsTypes = params.map(x => x.asTerm.typeSignature)
              val targs = (argsTypes :+ respte) map typeToTree
              targs
            }
            case _ => Nil // does not know how to extract the parameter types
          }
          Some(qualifier, args, targs)
        case _ => None
      }
    }

    def unsupported: (Position, String) => Unit =
      if (failCompilation) c.abort else c.warning

    def apply(tree: c.universe.Tree): (Tree, Seq[DSLFeature]) =
      (transform(tree), lifted.toSeq)
  }

}
