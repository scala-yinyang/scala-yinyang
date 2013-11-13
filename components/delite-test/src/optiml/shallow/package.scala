package optiml.shallow

package object ops {
  case class force() extends scala.annotation.StaticAnnotation
  trait Record
  def random[T]: T = ???
  def randomGaussian: Double = ???
  implicit class IntOps(i: Int) {
    def ::(o: Int): IndexVector = ???
  }
  def max(v1: Int, v2: Int): Int = math.max(v1, v2)
  def max(v1: Double, v2: Double): Double = math.max(v1, v2)
  def min(v1: Double, v2: Double): Double = math.min(v1, v2)
  def ceil(v: Double): Int = math.ceil(v).toInt
  def shuffle(__arg0: IndexVector): IndexVector = ???
  def diag[T: Manifest](__arg0: DenseMatrix[T]): DenseVector[T] = ???

  implicit class IndexVectorTuple2IndexVectorIndexVectorOpsCls(val self: Tuple2[IndexVector, IndexVector]) {
    def apply[T: Manifest](__arg1: (Int, Int) => T) = IndexVector.apply[T](self, __arg1)
  }

  def densematrix_raw_apply[T: Manifest](self: DenseMatrix[T], __arg0: Int): T = ???
  def densematrix_raw_update[T: Manifest](self: DenseMatrix[T], __arg0: Int, __arg1: T): Unit = ???

  def readVector(path: String): DenseVector[Double] = ???
  def readMatrix(path: String): DenseMatrix[Double] = ???
  def readVector[Elem: Manifest](path: String, schemaBldr: (DenseVector[String]) => Elem, delim: String = ("\\s+")): DenseVector[Elem] = ???
  def readMatrix[Elem: Manifest](path: String, schemaBldr: (String) => Elem, delim: String = ("\\s+")): DenseMatrix[Elem] = ???
  def sumRowsIf[A: Arith](start: Int, end: Int)(cond: Int => Boolean)(block: Int => DenseVectorView[A]): DenseVector[A] = ???
  def sumIf[A: Arith](start: Int, end: Int)(cond: Int => Boolean)(block: Int => A): A = ???
  def sum[A: Arith](start: Int, end: Int)(block: Int => A): A = ???
  def sum[A: Arith](arg: DenseVector[A]): A = ???
  def untilconverged[T](x: T, tol: Double = (0.001), minIter: Int = (1), maxIter: Int = (1000))(block: (T) => T): T = ???
  def untilconverged_buffered[T](x: T, tol: Double = (0.001), minIter: Int = (1), maxIter: Int = (1000))(block: (T) => T): T = ???
  def sigmoid(d: Double): Double = ???
  def VarSeq[T](v: T*): Seq[T] =
    v.toSeq
  def floor(d: Double): Int = ???
  def log(d: Double): Double = ???
  def dist(__arg0: DenseVector[Double], __arg1: DenseVector[Double], __arg2: DistanceMetric): Double = ???
  // implicit def tupleToDense2[T: Manifest](t: Tuple2[T, T]): DenseVector[T] = DenseVector[T](Seq((t._1), (t._2)))
  implicit def tupleToDense2yy[T: Manifest](t: Tuple2[T, T]): DenseVector[T] = DenseVector[T](Seq((t._1), (t._2)))
  // implicit def tupleToSeq4[T: Manifest](t: Tuple4[T, T, T, T]): Seq[T] = Seq((t._1), (t._2), (t._3), (t._4))
  @force implicit def viewToDense[T: Manifest](self: DenseVectorView[T]): DenseVector[T] = ???

  abstract class IndexWildcard
  object * extends IndexWildcard

  @force implicit def liftToIndexVectorTuple2IndexVectorIndexWildcardOpsClsYY(x: Tuple2[IndexVector, IndexWildcard]) = new IndexVectorTuple2IndexVectorIndexWildcardOpsCls(x)
  class IndexVectorTuple2IndexVectorIndexWildcardOpsCls(val self: Tuple2[IndexVector, IndexWildcard]) {
    def apply[T: Manifest](__arg1: (Int) => DenseVector[T]): DenseMatrix[T] = ???
  }

  trait DistanceMetric
  object EUC extends DistanceMetric
  object ABS extends DistanceMetric
  object SQUARE extends DistanceMetric

}