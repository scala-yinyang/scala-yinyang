package optiml.shallow

package object ops {
  def random[T]: T = ???
  def randomGaussian: Double = ???
  implicit class IntOps(i: Int) {
    def ::(o: Int): IndexVector = ???
  }
  def max(v1: Int, v2: Int): Int = math.max(v1, v2)
  def ceil(v: Double): Int = math.ceil(v).toInt

  implicit class IndexVectorTuple2IndexVectorIndexVectorOpsCls(val self: Tuple2[IndexVector, IndexVector]) {
    def apply[T: Manifest](__arg1: (Int, Int) => T) = IndexVector.apply[T](self, __arg1)
  }

  def densematrix_raw_apply[T: Manifest](self: DenseMatrix[T], __arg0: Int): T = ???
  def densematrix_raw_update[T: Manifest](self: DenseMatrix[T], __arg0: Int, __arg1: T): Unit = ???

  def readVector(path: String): DenseVector[Double] = ???
  def readMatrix(path: String): DenseMatrix[Double] = ???
  def sumRowsIf[A: Arith](start: Int, end: Int)(cond: Int => Boolean)(block: Int => DenseVectorView[A]): DenseVector[A] = ???
  def sum[A: Arith](start: Int, end: Int)(block: Int => A): A = ???
  def sum[A: Arith](arg: DenseVector[A]): A = ???
  def untilconverged[T](x: T, tol: Double = (0.001), minIter: Int = (1), maxIter: Int = (1000))(block: (T) => T): T = ???
  def sigmoid(d: Double): Double = ???
}