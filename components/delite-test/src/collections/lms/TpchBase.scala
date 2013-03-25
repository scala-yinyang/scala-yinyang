package collections.lms

import scala.virtualization.lms.common._

import collections.lms.TpchData._

/** Base class for the TPC-H benchmark query Q12 test. */
abstract class TpchBase
  extends BooleanOpsExp
  with DateOpsExp
  with EqualExpOpt
  with IfThenElseExpOpt
  with LiftBoolean
  with LiftNumeric
  with LiftString
  with MoreHashMapOpsExp
  with MoreIterableOpsExp
  with MoreListOpsExpOpt
  with MoreTupleOpsExp
  with NumericOpsExpOpt
  with StringOpsExp
  with CompileScala { self ⇒

  /** Method to be compiled by LMS. */
  def apply(params: Rep[(List[OrdersRow], List[LineitemRow], String, String, Date)]): Rep[List[(String, Int, Int)]] =
    apply(params._1, params._2, params._3, params._4, params._5)

  /** Signature of the method implementing the query. */
  def apply(
    orders: Rep[List[OrdersRow]],
    lineitem: Rep[List[LineitemRow]],
    shipmode1: Rep[String],
    shipmode2: Rep[String],
    date: Rep[Date]): Rep[List[(String, Int, Int)]]

  /** Code generator. */
  override val codegen =
    new ScalaGenBooleanOps with ScalaGenDateOps with ScalaGenEqual with ScalaGenHashMapOps with ScalaGenIfThenElse with ScalaGenMoreHashMapOps with ScalaGenMoreIterableOps with ScalaGenMoreListOps with ScalaGenMoreTupleOps with ScalaGenNumericOps with ScalaGenStringOps {
      val IR: self.type = self
    }
}
