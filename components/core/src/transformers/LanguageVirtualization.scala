package ch.epfl.yinyang
package transformers

import ch.epfl.yinyang._
import ch.epfl.yinyang.analysis._
import scala.reflect.macros.blackbox.Context
import language.experimental.macros
import scala.collection.mutable

/**
 * Converts Scala features that can not be overridden to method calls that can be given
 * arbitrary semantics.
 *
 * == Control Structures==
 * {{{
 *   if(c) t else e         =>       $ifThenElse(c, t, e)
 *   while(c) b             =>       $whileDo(c, b)
 *   do b while c           =>       $doWhile(c, b)
 *   return t               =>       $return(t)
 *   try c catch m finally f=>       $try(c, t: Throwable => t match m, f)
 *   throw t                =>       $throw(t)
 * }}}
 * ===Virtualization of `Any` methods===
 * {{{
 *   var x      = e                =>       var x = $varDef(e)
 *   lazy val x = e                =>       val x = $lazyValDef(e)
 *   x = t                         =>       $assign(x, t)
 *   x // depends on configuration =>       $read(x)
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
 * With `virtualizeFunctions`:
 * {{{
 *   x => e                 =>       $lam(x => e)
 *   f.apply(x)             =>       $app(f).apply(x)    // if `f` is a function object
 * }}}
 *
 * With `virtualizeValDef`:
 * {{{
 *   val x = b              =>       $valDef(b)
 * }}}
 *
 * With `virtualizeEquals`:
 * {{{
 *   t.toString             =>       infix_toString(t)
 *   t.hashCode             =>       infix_hashCode(t)
 *   t.equals t1            =>       infix_equals(t, t1)
 * }}}
 *
 * ===Restricted===
 * {{{
 *     class C { ... }
 *     trait T
 *     object O
 * }}}
 *
 */
trait LanguageVirtualization extends MacroModule with TransformationUtils
  with DataDefs with FreeIdentAnalysis {
  import c.universe._

  /**
   * Defines if functions are virtualized.
   */
  val virtualizeFunctions: Boolean

  /**
   * Defines if we should restrict class, trait, and object definitions. In macros
   * we do not want to reject programs with nested definitions.
   */
  val restrictDefinitions: Boolean = true

  /**
   * Appends names to value bindings. This is used for debugging of generated code
   * in some DSLs.
   */
  val nameBindings: Boolean = false

  /**
   * Defines virtualizaition of non-final universal methods (e.g., `toString`).
   */
  val virtualizeEquals: Boolean = true

  /**
   * Defines whether we virtualize value definitions.
   */
  val virtualizeValDef: Boolean = false

  /**
   * Defines a prefix for all virtualized methods.
   */
  val prefix = "$"

  def virtualize(t: Tree): (Tree, Seq[DSLFeature]) = VirtualizationTransformer(t)

  object VirtualizationTransformer {
    def apply(tree: Tree) = {
      val freeVars = freeVariables(tree)
      val t = new VirtualizationTransformer(freeVars).apply(tree)
      log("(virtualized, Seq[Features]): " + t, 2)
      t
    }
  }

  private class VirtualizationTransformer(val freeVars: List[Tree] = Nil) extends Transformer {
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
          liftFeature(None, prefix + "lam", List(Function(vparams, tree)), Nil, trans = x => x)

        case FunctionApply(qualifier, args, targs) if virtualizeFunctions =>
          Apply(Select(liftFeature(None, prefix + "app", List(qualifier), targs), TermName("apply")), args map transform)

        case t @ If(cond, thenBr, elseBr) =>
          liftFeature(None, prefix + "ifThenElse", List(cond, thenBr, elseBr))

        case Return(e) =>
          liftFeature(None, prefix + "return", List(e))

        case LabelDef(sym, List(), If(cond, Block(body :: Nil, Apply(Ident(label),
          List())), Literal(Constant(())))) if label == sym => // while(){}
          liftFeature(None, prefix + "whileDo", List(cond, body))

        case LabelDef(sym, List(), Block(body :: Nil, If(cond, Apply(Ident(label),
          List()), Literal(Constant(()))))) if label == sym => // do while(){}
          liftFeature(None, prefix + "doWhile", List(cond, body))

        case Try(block, catches, finalizer) =>
          val arg = Ident(TermName("x"))
          val etaExpandedCatch = Function(
            List(ValDef(Modifiers(Flag.PARAM), TermName("x"), Ident(TypeName("Any")), EmptyTree)),
            if (catches == Nil) arg else Match(arg, catches))
          val finalizerTree = if (finalizer == EmptyTree) q"null" else finalizer
          val tparams = if (tree.tpe == null) Nil else List(TypeTree(tree.tpe))
          liftFeature(None, prefix + "try", List(block, etaExpandedCatch, finalizerTree), tparams)

        case Throw(expr) =>
          liftFeature(None, prefix + "throw", List(expr))

        case MultipleTypeApply(Select(New(nme), termNames.CONSTRUCTOR), targs, argss) =>
          method(None, prefix + "new_" + nme.toString, argss.map(_.map(transform)), targs)

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
          ValDef(mods, sym, tpt, liftFeature(None, prefix + "varDef", List(rhs) ++ named(sym)))

        case Assign(lhs, rhs) =>
          liftFeature(None, prefix + "assign", List(lhs, transform(rhs)), Nil, x => x)

        case ValDef(mods, sym, tpt, rhs) if mods.hasFlag(Flag.LAZY) =>
          ValDef(mods, sym, tpt, liftFeature(None, prefix + "lazyValDef", List(rhs) ++ named(sym)))

        case ValDef(mods, sym, tpt, rhs) =>
          val newRhs =
            if (virtualizeValDef) liftFeature(None, prefix + "valDef", List(rhs) ++ named(sym))
            else transform(rhs)

          ValDef(mods, sym, tpt, newRhs)

        case Ident(x) if (tree.symbol.isTerm && !(freeVars contains tree) && (
          tree.symbol.asTerm.isVar ||
          tree.symbol.asTerm.isLazy ||
          (!tree.symbol.isParameter && tree.symbol.asTerm.isVal && virtualizeValDef))) =>
          liftFeature(None, prefix + "read", List(tree), Nil, x => x)

        //
        // Universal methods virtualization
        //
        case Apply(Select(qualifier, TermName("$eq$eq")), List(arg)) =>
          liftFeature(None, prefix + "infix_$eq$eq", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("$bang$eq")), List(arg)) =>
          liftFeature(None, prefix + "infix_$bang$eq", List(qualifier, arg))

        case Apply(lhs @ Select(qualifier, TermName("$hash$hash")), List()) =>
          liftFeature(None, prefix + "infix_$hash$hash", List(qualifier))

        case Apply(lhs @ Select(qualifier, TermName("equals")), List(arg)) if virtualizeEquals =>
          liftFeature(None, prefix + "infix_equals", List(qualifier, arg))

        case Apply(lhs @ Select(qualifier, TermName("hashCode")), List()) =>
          liftFeature(None, prefix + "infix_hashCode", List(qualifier))

        case TypeApply(Select(qualifier, TermName("asInstanceOf")), targs) =>
          liftFeature(None, prefix + "infix_asInstanceOf", List(qualifier), targs)

        case TypeApply(Select(qualifier, TermName("isInstanceOf")), targs) =>
          liftFeature(None, prefix + "infix_isInstanceOf", List(qualifier), targs)

        case TypeApply(Select(qualifier, TermName("getClass")), targs) =>
          liftFeature(None, prefix + "infix_getClass", List(qualifier), targs)

        case Apply(lhs @ Select(qualifier, TermName("toString")), List()) =>
          liftFeature(None, prefix + "infix_toString", List(qualifier))

        case Apply(lhs @ Select(qualifier, TermName("eq")), List(arg)) =>
          liftFeature(None, prefix + "infix_eq", List(qualifier, arg))

        case Apply(lhs @ Select(qualifier, TermName("ne")), List(arg)) =>
          liftFeature(None, prefix + "infix_ne", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("notify")), List()) =>
          liftFeature(None, prefix + "infix_notify", List(qualifier))

        case Apply(Select(qualifier, TermName("notifyAll")), List()) =>
          liftFeature(None, prefix + "infix_notifyAll", List(qualifier))

        case Apply(Select(qualifier, TermName("synchronized")), List(arg)) =>
          liftFeature(None, prefix + "infix_synchronized", List(qualifier, arg))

        case Apply(TypeApply(Select(qualifier, TermName("synchronized")), targs), List(arg)) =>
          liftFeature(None, prefix + "infix_synchronized", List(qualifier, arg), targs)

        case Apply(Select(qualifier, TermName("wait")), List()) =>
          liftFeature(None, prefix + "infix_wait", List(qualifier))

        case Apply(Select(qualifier, TermName("wait")), List(arg)
          ) if arg.tpe =:= typeOf[Long] =>
          liftFeature(None, prefix + "infix_wait", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("wait")), List(arg0, arg1)
          ) if arg0.tpe =:= typeOf[Long] && arg1.tpe =:= typeOf[Int] =>
          liftFeature(None, prefix + "infix_wait", List(qualifier, arg0, arg1))

        case Typed(x, Ident(typeNames.WILDCARD_STAR)) =>
          Typed(liftFeature(None, prefix + "castVarArg", List(x)), Ident(typeNames.WILDCARD_STAR))

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

    def apply(tree: c.universe.Tree): (Tree, Seq[DSLFeature]) =
      (transform(tree), lifted.toSeq)
  }

}
