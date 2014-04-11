package reptest

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object test {

  def failure: Any = macro failureImpl
  def failureImpl(c: Context): c.Expr[Any] = {
    import c.universe._

    def dslTrait(dslName: String) = {
      val names = dslName.split("\\.").toList.reverse
      assert(names.length >= 1, "DSL trait name must be in the valid format. DSL trait name is " + dslName)

      val tpeName = TypeName(names.head)
      names.tail.reverse match {
        case head :: tail =>
          Select(tail.foldLeft[Tree](Ident(TermName(head)))((tree, name) => Select(tree, TermName(name))), tpeName)
        case Nil =>
          Ident(tpeName)
      }
    }

    def composeDSL(transformedBody: Tree) =
      // class MyDSL extends DSL {
      ClassDef(Modifiers(), TypeName("eval"), List(), Template(
        List(dslTrait("failure.FailureCake")),
        emptyValDef,
        List(
          DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(),
            Block(List(Apply(Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR), List())), Literal(Constant(())))),
          // def main = {
          DefDef(Modifiers(), TermName("main"), List(), List(List()), Ident(TypeName("Any")), transformedBody))))
    //     }
    // }

    def constructor = Apply(Select(New(Ident(TypeName("eval"))), termNames.CONSTRUCTOR), List())

    c.eval(c.Expr[Any](
      c.untypecheck(Block(composeDSL(Literal(Constant(1))), constructor))))

    c.Expr[Any](Literal(Constant(1)))
  }

}