package collections.lms

import scala.collection.mutable.HashMap

import scala.virtualization.lms.common._

/** Base class for the Mnemonics test. */
abstract class MnemonicsBase
  extends BooleanOpsExp
  with DateOpsExp
  with EqualExpOpt
  with FunctionsExternalDef
  with IfThenElseExpOpt
  with LiftBoolean
  with LiftNumeric
  with LiftString
  with MoreHashMapOpsExp
  with MoreIterableOpsExp
  with MoreListOpsExpOpt
  with MoreTupleOpsExp
  with NumericOpsExpOpt
  with RangeOpsExp
  with SeqOpsExp
  with StringOpsExp
  with CompileScala { self ⇒

  type LString = scala.List[Char]

  /** Method to be compiled by LMS. */
  def apply(params: Rep[(HashMap[Char, LString], List[LString], LString)]): Rep[List[List[LString]]] =
    apply(params._1, params._2, params._3)

  /** Signature of the method implementing the test. */
  def apply(
    mnemonics: Rep[HashMap[Char, LString]],
    words: Rep[List[LString]],
    number: Rep[LString]): Rep[List[List[LString]]]

  /** Code generator. */
  override val codegen =
    new ScalaGenBooleanOps with ScalaGenDateOps with ScalaGenEqual with ScalaGenFunctionsExternal with ScalaGenIfThenElse with ScalaGenMoreHashMapOps with ScalaGenMoreIterableOps with ScalaGenMoreListOps with ScalaGenMoreTupleOps with ScalaGenNumericOps with ScalaGenRangeOps with ScalaGenSeqOps with ScalaGenStringOps {
      val IR: self.type = self
    }
}
