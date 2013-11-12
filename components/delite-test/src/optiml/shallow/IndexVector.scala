package optiml.shallow.ops

import scala.math.Ordering.Implicits._
import scala.math.Numeric.Implicits._
import ch.epfl.lamp.autolifter.library._
import ch.epfl.lamp.autolifter.annotations._
import ForgeArray._
import ForgeArrayBuffer._
import Numeric._
import Arith._
import Stringable._

/**
 * Operations
 */

object IndexVector {
  import DenseVector._
  import DenseMatrix._
  def apply(__arg0: Int, __arg1: Int): IndexVector = /*redirect*/ IndexVector(__arg0, __arg1, (true))
  def apply(__arg0: Int, __arg1: Int, __arg2: Boolean): IndexVector = /*allocates*/ new IndexVector(array_empty_imm[Int]((0)), __arg0, __arg1, __arg2, (true))

  def apply(__arg0: DenseVector[Int]): IndexVector = /*redirect*/ IndexVector(__arg0, __arg0.isRow)
  def apply(__arg0: DenseVector[Int], __arg1: Boolean): IndexVector = /*allocates*/ new IndexVector(indexvector_copyarray(__arg0), (0), (0), __arg1, (false))

  def apply[T: Manifest](__arg0: Tuple2[IndexVector, IndexVector], __arg1: (Int, Int) => T) = {
    val (rowIndices, colIndices) = __arg0
    // can fuse with flat matrix loops
    val v = (0 :: rowIndices.length * colIndices.length).toDense
    val indices = densematrix_fromarray(densevector_raw_data(v), rowIndices.length, colIndices.length)
    indices map { i =>
      val rowIndex = i / colIndices.length
      val colIndex = i % colIndices.length
      __arg1(rowIndex, colIndex)
    }
  }

  protected[ops] def indexvector_fromarray(__arg0: ForgeArray[Int], __arg1: Boolean): IndexVector = /*allocates*/ new IndexVector(__arg0, (0), (0), __arg1, (false))

  protected[ops] def indexvector_copyarray(__arg0: DenseVector[Int]): ForgeArray[Int] = Delite.composite({
    val d = array_empty[Int](__arg0.length)
    __arg0.indices foreach { i => d(i) = __arg0(i) }
    d.unsafeImmutable
  })

  protected[ops] def indexvector_start(self: IndexVector): Int = Forge.getter(self, self._start)

  protected[ops] def indexvector_end(self: IndexVector): Int = Forge.getter(self, self._end)

  protected[ops] def indexvector_raw_data(self: IndexVector): ForgeArray[Int] = Forge.getter(self, self._data)

  protected[ops] def indexvector_is_range(self: IndexVector): Boolean = Forge.getter(self, self._isRange)

  protected[ops] def indexvector_is_row(self: IndexVector): Boolean = Forge.getter(self, self._isRow)

  protected[ops] def indexvector_illegalalloc(self: IndexVector, __arg1: Int): Nothing = Delite.composite(fatal("IndexVectors cannot be allocated from a parallel op"))

  protected[ops] def indexvector_illegalupdate(self: IndexVector, __arg1: Int, __arg2: Int): Nothing = Delite.composite(fatal("IndexVectors cannot be updated"))

  protected[ops] def zeroT(): Int = Delite.composite(0.asInstanceOf[Int])

  protected[ops] def indexvector_densevector_filter_map[R: Manifest](self: IndexVector, __arg1: (Int) => Boolean, __arg2: (Int) => R): DenseVector[R] = Delite.filter[Int, R, DenseVector[R]](self, (e => __arg1(e)), (e => __arg2(e)))

}

class IndexVector(___data: ForgeArray[Int], ___start: Int, ___end: Int, ___isRow: Boolean, ___isRange: Boolean) extends ParallelCollection[Int] { self =>
  var _data = ___data
  var _start = ___start
  var _end = ___end
  var _isRow = ___isRow
  var _isRange = ___isRange

  import IndexVector._
  import DenseVector._
  import DenseMatrix._
  def length(): Int = Delite.composite({
    if (indexvector_is_range(self)) {
      indexvector_end(self) - indexvector_start(self)
    } else {
      array_length(indexvector_raw_data(self))
    }
  })

  def isRow(): Boolean = Forge.getter(self, self._isRow)

  def apply(__arg0: Int): Int = Delite.composite({
    if (indexvector_is_range(self)) {
      indexvector_start(self) + __arg0
    } else {
      indexvector_raw_data(self).apply(__arg0)
    }
  })

  def slice(start: Int, end: Int): IndexVector = Delite.composite({
    if (indexvector_is_range(self)) {

      IndexVector(start, end, self.isRow)
    } else {
      IndexVector(densevector_fromarray(indexvector_raw_data(self), self.isRow).slice(start, end))
    }
  })

  def t(): IndexVector = /*allocates*/ new IndexVector(indexvector_raw_data(self), indexvector_start(self), indexvector_end(self), !(indexvector_is_row(self)), indexvector_is_range(self))

  def toDense(): DenseVector[Int] = Delite.composite(self.map(e => e))

  def toBoolean()(implicit conv: (Int) => Boolean): DenseVector[Boolean] = Delite.map[Int, Boolean, DenseVector[Boolean]](self, (conv))

  def toDouble()(implicit conv: (Int) => Double): DenseVector[Double] = Delite.map[Int, Double, DenseVector[Double]](self, (conv))

  def toFloat()(implicit conv: (Int) => Float): DenseVector[Float] = Delite.map[Int, Float, DenseVector[Float]](self, (conv))

  def toInt()(implicit conv: (Int) => Int): DenseVector[Int] = Delite.map[Int, Int, DenseVector[Int]](self, (conv))

  def indices(): IndexVector = Delite.composite(IndexVector((0), self.length, self.isRow))

  def isEmpty(): Boolean = Delite.single(self.length == 0)

  def first(): Int = Delite.single(self(0))

  def last(): Int = Delite.single(self(self.length - 1))

  def drop(__arg0: Int): IndexVector = Delite.composite(self.slice(__arg0, self.length))

  def take(__arg0: Int): IndexVector = Delite.composite(self.slice(0, __arg0))

  def contains(__arg0: Int): Boolean = Delite.single({
    var found = false
    var i = 0
    while (i < self.length && !found) {
      if (self(i) == __arg0) {
        found = true
      }
      i += 1
    }
    found
  })

  def distinct(): DenseVector[Int] = Delite.single({
    val out = DenseVector[Int](0, self.isRow)
    for (i <- 0 until self.length) {

      if (!out.contains(self(i))) out <<= self(i)
    }
    out.unsafeImmutable
  })

  def makeString(): String = Delite.single({
    var s = ""
    if (self.length == 0) {
      "[ ]"
    } else if (self.isRow) {
      s = s + "["
      for (i <- 0 until self.length - 1) {
        s = s + self(i).makeStr + " "
      }
      s = s + self(self.length - 1).makeStr
      s = s + "]"
    } else {
      for (i <- 0 until self.length - 1) {
        s = s + "[" + self(i).makeStr + "]\n"
      }
      s = s + "[" + self(self.length - 1).makeStr + "]"
    }
    s
  })

  override def toString(): String = Delite.single({
    var s = ""
    if (self.length == 0) {
      "[ ]"
    } else if (self.isRow) {
      s = s + "["
      for (i <- 0 until self.length - 1) {
        s = s + self(i) + " "
      }
      s = s + self(self.length - 1)
      s = s + "]"
    } else {
      for (i <- 0 until self.length - 1) {
        s = s + "[" + self(i) + "]\n"
      }
      s = s + "[" + self(self.length - 1) + "]"
    }
    s
  })

  @simple def pprint(): Unit = Delite.composite(println(self.makeStr))

  def +(__arg0: DenseVector[Int]): DenseVector[Int] = Delite.zip[Int, Int, Int, DenseVector[Int]](self, __arg0, ((a, b) => a + b))

  def -(__arg0: DenseVector[Int]): DenseVector[Int] = Delite.zip[Int, Int, Int, DenseVector[Int]](self, __arg0, ((a, b) => a - b))

  def *(__arg0: DenseVector[Int]): DenseVector[Int] = Delite.zip[Int, Int, Int, DenseVector[Int]](self, __arg0, ((a, b) => a * b))

  def *:*(__arg0: DenseVector[Int]): Int = Delite.composite({
    if (self.length != __arg0.length) fatal("dimension mismatch: vector dot product")
    (self * __arg0).sum
  })

  def **(__arg0: DenseVector[Int]): DenseMatrix[Int] = Delite.composite({
    if (self.isRow || !__arg0.isRow) fatal("dimension mismatch: vector outer product")
    val out = DenseMatrix[Int](self.length, __arg0.length)
    for (i <- 0 until self.length) {
      for (j <- 0 until __arg0.length) {
        out(i, j) = self(i) * __arg0(j)
      }
    }
    out.unsafeImmutable
  })

  def /(__arg0: DenseVector[Int]): DenseVector[Int] = Delite.zip[Int, Int, Int, DenseVector[Int]](self, __arg0, ((a, b) => a / b))

  def +(__arg0: DenseVectorView[Int]): DenseVector[Int] = Delite.zip[Int, Int, Int, DenseVector[Int]](self, __arg0, ((a, b) => a + b))

  def -(__arg0: DenseVectorView[Int]): DenseVector[Int] = Delite.zip[Int, Int, Int, DenseVector[Int]](self, __arg0, ((a, b) => a - b))

  def *(__arg0: DenseVectorView[Int]): DenseVector[Int] = Delite.zip[Int, Int, Int, DenseVector[Int]](self, __arg0, ((a, b) => a * b))

  def *:*(__arg0: DenseVectorView[Int]): Int = Delite.composite({
    if (self.length != __arg0.length) fatal("dimension mismatch: vector dot product")
    (self * __arg0).sum
  })

  def **(__arg0: DenseVectorView[Int]): DenseMatrix[Int] = Delite.composite({
    if (self.isRow || !__arg0.isRow) fatal("dimension mismatch: vector outer product")
    val out = DenseMatrix[Int](self.length, __arg0.length)
    for (i <- 0 until self.length) {
      for (j <- 0 until __arg0.length) {
        out(i, j) = self(i) * __arg0(j)
      }
    }
    out.unsafeImmutable
  })

  def /(__arg0: DenseVectorView[Int]): DenseVector[Int] = Delite.zip[Int, Int, Int, DenseVector[Int]](self, __arg0, ((a, b) => a / b))

  def zip[B: Manifest, R: Manifest](__arg0: DenseVector[B])(__arg1: (Int, B) => R): DenseVector[R] = Delite.zip[Int, B, R, DenseVector[R]](self, __arg0, ((a, b) => __arg1(a, b)))

  def zip[B: Manifest, R: Manifest](__arg0: DenseVectorView[B])(__arg1: (Int, B) => R): DenseVector[R] = Delite.zip[Int, B, R, DenseVector[R]](self, __arg0, ((a, b) => __arg1(a, b)))

  def +(__arg0: Int): DenseVector[Int] = Delite.map[Int, Int, DenseVector[Int]](self, (e => e + __arg0))

  def -(__arg0: Int): DenseVector[Int] = Delite.map[Int, Int, DenseVector[Int]](self, (e => e - __arg0))

  def *(__arg0: Int): DenseVector[Int] = Delite.map[Int, Int, DenseVector[Int]](self, (e => e * __arg0))

  def *(__arg0: DenseMatrix[Int]): DenseVector[Int] = Delite.composite({
    if (!self.isRow) fatal("dimension mismatch: vector * matrix")
    __arg0.t.mapRowsToVector { row => self *:* row }
  })

  def /(__arg0: Int): DenseVector[Int] = Delite.map[Int, Int, DenseVector[Int]](self, (e => e / __arg0))

  def abs(): DenseVector[Int] = Delite.map[Int, Int, DenseVector[Int]](self, (e => e.abs))

  def exp(): DenseVector[Int] = Delite.map[Int, Int, DenseVector[Int]](self, (e => e.exp))

  def log(): DenseVector[Int] = Delite.map[Int, Int, DenseVector[Int]](self, (e => e.log))

  def sum(): Int = Delite.reduce[Int](self, (zeroT), ((a, b) => a + b))

  def mean()(implicit conv: (Int) => Double): Double = Delite.composite(self.map(conv).sum / self.length)

  def min(): Int = Delite.reduce[Int](self, (self(0)), ((a, b) => if (a < b) a else b))

  def max(): Int = Delite.reduce[Int](self, (self(0)), ((a, b) => if (a > b) a else b))

  def minIndex(): Int = Delite.single({
    var min = self(0)
    var minIndex = 0
    for (i <- 0 until self.length) {
      if (self(i) < min) {
        min = self(i)
        minIndex = i
      }
    }
    minIndex
  })

  def maxIndex(): Int = Delite.single({
    var max = self(0)
    var maxIndex = 0
    for (i <- 0 until self.length) {
      if (self(i) > max) {
        max = self(i)
        maxIndex = i
      }
    }
    maxIndex
  })

  def map[R: Manifest](__arg0: (Int) => R): DenseVector[R] = Delite.map[Int, R, DenseVector[R]](self, (e => __arg0(e)))

  def reduce(__arg0: (Int, Int) => Int): Int = Delite.reduce[Int](self, (zeroT), ((a, b) => __arg0(a, b)))

  def filter(__arg0: (Int) => Boolean): DenseVector[Int] = Delite.filter[Int, Int, DenseVector[Int]](self, (e => __arg0(e)), (e => e))

  def foreach(__arg0: (Int) => Unit): Unit = Delite.foreach[Int](self, (e => __arg0(e)))

  def find(__arg0: (Int) => Boolean): IndexVector = Delite.composite(IndexVector(self.indices.filter(i => __arg0(self(i)))))

  def count(__arg0: (Int) => Boolean): Int = Delite.composite((indexvector_densevector_filter_map(self, __arg0, (e: Int) => 1)).sum)

  def partition(pred: (Int) => Boolean): Tuple2[DenseVector[Int], DenseVector[Int]] = Delite.single({
    val outT = DenseVector[Int](0, self.isRow)
    val outF = DenseVector[Int](0, self.isRow)
    for (i <- 0 until self.length) {
      val x = self(i)
      if (pred(x)) outT <<= x
      else outF <<= x
    }
    (outT.unsafeImmutable, outF.unsafeImmutable)
  })

  def flatMap[R: Manifest](__arg0: (Int) => DenseVector[R]): DenseVector[R] = Delite.composite(DenseVector.flatten(self.map(__arg0)))

  def scan[R: Manifest](zero: R)(__arg1: (R, Int) => R): DenseVector[R] = Delite.single({
    val out = DenseVector[R](self.length, self.isRow)
    out(0) = __arg1(zero, self(0))
    var i = 1
    while (i < self.length) {
      out(i) = __arg1(out(i - 1), self(i))
      i += 1
    }
    out.unsafeImmutable
  })

  def prefixSum(): DenseVector[Int] = Delite.composite(self.scan(zeroT)((a, b) => a + b))

  def apply[T: Manifest](__arg1: (Int) => T): DenseVector[T] = Delite.composite(self.map(__arg1))

}
