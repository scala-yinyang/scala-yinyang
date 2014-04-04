package ch.epfl.styx

import collection.mutable._
import collection.mutable
import ch.epfl.yinyang.api._
import reflect.runtime.universe.{ TypeTag, Symbol }

case class Hole[T](tpe: TypeTag[T], symbolId: Int) extends Node
case class Mixed[T](v: T, h: Exp[Any]) extends Node
case class Const[T](v: T) extends Node

/**
 * Base trait for reification.
 */
trait ReificationBase extends BaseYinYangTypeTag {
  type R[+T] = Exp[T]
  val compiler: Context

  /* Casting to escape the type system */
  implicit def toSymbol[T](x: Node): Exp[T] = compiler.createNode(x).asInstanceOf[Exp[T]]

  implicit def genericLifter[T: TypeTag]: LiftEvidence[T, R[T]] = new LiftEvidence[T, R[T]] {
    def hole(tpe: TypeRep[T], symbolId: Int): R[T] = Hole(tpe, symbolId)
    def lift(v: T): R[T] = Const(v)
    override def mixed(v: T, hole: R[T]): R[T] = Mixed(v, hole)
  }
}

// We do this with the LMS lifter
case class Plus(lhs: SymLike, rhs: SymLike) extends Node

trait Arithmetics extends ReificationBase {
  implicit class IntOps(lhs: R[Int]) {
    def +(rhs: R[Int]): R[Int] = Plus(lhs, rhs)
  }
}

trait SymLike

trait Exp[+T] extends SymLike
case class Sym() extends Exp[Any] {
  val id = 1
}

trait Node

trait Context {
  val symbolTable: mutable.Map[Sym, Node] = new LinkedHashMap[Sym, Node]()

  def createNode(n: Node) = symbolTable.put(Sym(), n)
  def fresh: Sym = Sym()
}

class MyCompiler extends Context

// A DSL
final class MyDSL extends ReificationBase {
  val compiler: Context = new MyCompiler
  def compilationVars(symbols: List[Symbol]): List[VarType] = ???
}