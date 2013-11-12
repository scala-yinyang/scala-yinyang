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

object DenseMatrix {
  @mutable def apply[T: Manifest](__arg0: Int, __arg1: Int): DenseMatrix[T] = /*allocates*/ new DenseMatrix[T](__arg0, __arg1, array_empty[T](__arg0 * __arg1))

  def apply[T: Manifest](__arg0: DenseVector[DenseVector[T]])(implicit d: DummyImplicit): DenseMatrix[T] = Delite.composite({
    val numRows = __arg0.length
    val numCols = __arg0(0).length
    (0 :: numRows, 0 :: numCols) { (i, j) => __arg0(i).apply(j) }
  })

  def apply[T: Manifest](__arg0: DenseVector[DenseVectorView[T]]): DenseMatrix[T] = Delite.composite({
    val numRows = __arg0.length
    val numCols = __arg0(0).length
    (0 :: numRows, 0 :: numCols) { (i, j) => __arg0(i).apply(j) }
  })

  def apply[T: Manifest](__arg0: Seq[DenseVector[T]]): DenseMatrix[T] = Delite.single({
    val out = DenseMatrix[T](0, 0)

    for (i: Int <- scala.collection.immutable.Range(0, __arg0.length)) {
      out <<= __arg0(i)
    }
    out.unsafeImmutable
  })

  def diag[T: Arith: Manifest](__arg0: Int, __arg1: DenseVector[T]): DenseMatrix[T] = Delite.single({
    densematrix_fromfunc(__arg0, __arg0, (i, j) =>
      if (i == j) __arg1(i)
      else implicitly[Arith[T]].empty)
  })

  def identity(__arg0: Int, __arg1: Int): DenseMatrix[Double] = Delite.single({
    densematrix_fromfunc(__arg0, __arg1, (i, j) =>
      if (i == j) 1.0
      else 0.0)
  })

  def identity(__arg0: Int): DenseMatrix[Double] = /*redirect*/ DenseMatrix.identity(__arg0, __arg0)
  protected[ops] def densematrix_fromarray[T: Manifest](__arg0: ForgeArray[T], __arg1: Int, __arg2: Int): DenseMatrix[T] = /*allocates*/ new DenseMatrix[T](__arg1, __arg2, __arg0)

  protected[ops] def densematrix_fromfunc[T: Manifest](__arg0: Int, __arg1: Int, __arg2: (Int, Int) => T): DenseMatrix[T] = Delite.composite((0 :: __arg0, 0 :: __arg1) { (i, j) => __arg2(i, j) })

  def zeros(__arg0: Int, __arg1: Int): DenseMatrix[Double] = Delite.composite(densematrix_fromfunc(__arg0, __arg1, (i, j) => 0.0))

  def zerosf(__arg0: Int, __arg1: Int): DenseMatrix[Float] = Delite.composite(densematrix_fromfunc(__arg0, __arg1, (i, j) => 0f))

  def ones(__arg0: Int, __arg1: Int): DenseMatrix[Double] = Delite.composite(densematrix_fromfunc(__arg0, __arg1, (i, j) => 1.0))

  def onesf(__arg0: Int, __arg1: Int): DenseMatrix[Float] = Delite.composite(densematrix_fromfunc(__arg0, __arg1, (i, j) => 1f))

  def rand(__arg0: Int, __arg1: Int): DenseMatrix[Double] = Delite.composite(densematrix_fromfunc(__arg0, __arg1, (i, j) => random[Double]))

  def randf(__arg0: Int, __arg1: Int): DenseMatrix[Float] = Delite.composite(densematrix_fromfunc(__arg0, __arg1, (i, j) => random[Float]))

  def randn(__arg0: Int, __arg1: Int): DenseMatrix[Double] = Delite.composite(densematrix_fromfunc(__arg0, __arg1, (i, j) => randomGaussian))

  def randnf(__arg0: Int, __arg1: Int): DenseMatrix[Float] = Delite.composite(densematrix_fromfunc(__arg0, __arg1, (i, j) => randomGaussian.toFloat))

  protected[ops] def densematrix_index[T: Manifest](self: DenseMatrix[T], __arg1: Int, __arg2: Int): Int = Delite.composite(__arg1 * self.numCols + __arg2)

  protected[ops] def densematrix_raw_data[T: Manifest](self: DenseMatrix[T]): ForgeArray[T] = Forge.getter(self, self._data)

  protected[ops] def densematrix_set_raw_data[T: Manifest](@write self: DenseMatrix[T], __arg1: ForgeArray[T]): Unit = Forge.setter(self, self._data, __arg1)

  protected[ops] def densematrix_set_numrows[T: Manifest](@write self: DenseMatrix[T], __arg1: Int): Unit = Forge.setter(self, self._numRows, __arg1)

  protected[ops] def densematrix_set_numcols[T: Manifest](@write self: DenseMatrix[T], __arg1: Int): Unit = Forge.setter(self, self._numCols, __arg1)

  protected[ops] def densematrix_insertspace[T: Manifest](@write self: DenseMatrix[T], pos: Int, len: Int): Unit = Delite.single({
    if (pos < 0 || pos > self.size) fatal("DenseMatrix IndexOutOfBounds")
    densematrix_ensureextra(self, len)
    val d = densematrix_raw_data(self)
    array_copy(d, pos, d, pos + len, self.size - pos)
  })

  protected[ops] def densematrix_ensureextra[T: Manifest](@write self: DenseMatrix[T], extra: Int): Unit = Delite.single({
    val data = densematrix_raw_data(self)
    if (array_length(data) - self.size < extra) {
      densematrix_realloc(self, self.size + extra)
    }
  })

  protected[ops] def densematrix_realloc[T: Manifest](@write self: DenseMatrix[T], minLen: Int): Unit = Delite.single({
    val data = densematrix_raw_data(self)
    var n = max(4, array_length(data) * 2)
    while (n < minLen) n = n * 2
    val d = array_empty[T](n)
    array_copy(data, 0, d, 0, self.size)
    densematrix_set_raw_data(self, d.unsafeImmutable)
  })

  protected[ops] def densematrix_raw_alloc[T: Manifest, R: Manifest](self: DenseMatrix[T], __arg1: Int): DenseMatrix[R] = Delite.composite(DenseMatrix[R](self.numRows, self.numCols))

}

class DenseMatrix[T: Manifest](___numRows: Int, ___numCols: Int, ___data: ForgeArray[T]) extends ParallelCollection[T] { self =>
  var _numRows = ___numRows
  var _numCols = ___numCols
  var _data = ___data

  import DenseMatrix._
  def toBoolean()(implicit conv: (T) => Boolean): DenseMatrix[Boolean] = Delite.map[T, Boolean, DenseMatrix[Boolean]](self, (conv))

  def toDouble()(implicit conv: (T) => Double): DenseMatrix[Double] = Delite.map[T, Double, DenseMatrix[Double]](self, (conv))

  def toFloat()(implicit conv: (T) => Float): DenseMatrix[Float] = Delite.map[T, Float, DenseMatrix[Float]](self, (conv))

  def toInt()(implicit conv: (T) => Int): DenseMatrix[Int] = Delite.map[T, Int, DenseMatrix[Int]](self, (conv))

  def numRows(): Int = Forge.getter(self, self._numRows)

  def numCols(): Int = Forge.getter(self, self._numCols)

  def size(): Int = Delite.composite(self.numRows * self.numCols)

  def apply(__arg0: Int, __arg1: Int): T = Delite.composite(array_apply(densematrix_raw_data(self), densematrix_index(self, __arg0, __arg1)))

  def apply(__arg0: Int): DenseVectorView[T] = Delite.composite(self.getRow(__arg0))

  def apply(__arg0: IndexVector): DenseMatrix[T] = Delite.composite({
    if (__arg0.isRow) {
      DenseMatrix(__arg0.map(i => self.getCol(i))).t
    } else {
      DenseMatrix(__arg0.map(i => self(i)))
    }
  })

  def rowIndices(): IndexVector = Delite.composite(IndexVector(0, self.numRows, false))

  def colIndices(): IndexVector = Delite.composite(IndexVector(0, self.numCols))

  def vview(__arg0: Int, __arg1: Int, __arg2: Int, __arg3: Boolean): DenseVectorView[T] = Delite.composite(DenseVectorView[T](densematrix_raw_data(self).unsafeImmutable, __arg0, __arg1, __arg2, __arg3))

  def getRow(__arg0: Int): DenseVectorView[T] = Delite.composite(self.vview(__arg0 * self.numCols, 1, self.numCols, true))

  def getCol(__arg0: Int): DenseVectorView[T] = Delite.composite(self.vview(__arg0, self.numCols, self.numRows, false))

  def slice(startRow: Int, endRow: Int, startCol: Int, endCol: Int): DenseMatrix[T] = Delite.single({
    val out = DenseMatrix[T](endRow - startRow, endCol - startCol)
    for (i <- startRow until endRow) {
      for (j <- startCol until endCol) {
        out(i - startRow, j - startCol) = self(i, j)
      }
    }
    out.unsafeImmutable
  })

  def sliceRows(start: Int, end: Int): DenseMatrix[T] = Delite.single({
    val out = DenseMatrix[T](end - start, self.numCols)
    for (i <- start until end) {
      for (j <- 0 until self.numCols) {
        out(i - start, j) = self(i, j)
      }
    }
    out.unsafeImmutable
  })

  def sliceCols(start: Int, end: Int): DenseMatrix[T] = Delite.single({
    val out = DenseMatrix[T](self.numRows, end - start)
    for (i <- 0 until self.numRows) {
      for (j <- start until end) {
        out(i, j - start) = self(i, j)
      }
    }
    out.unsafeImmutable
  })

  @simple def pprint()(implicit __imp0: Stringable[T]): Unit = Delite.composite(println(self.makeStr))

  def makeString()(implicit __imp0: Stringable[T]): String = Delite.single({
    var s = ""
    for (i <- 0 until self.numRows - 1) {
      s = s + self(i).makeStr + "\n"
    }
    if (self.numRows > 0)
      s + self(self.numRows - 1).makeStr
    else "[ ]"
  })

  override def toString(): String = Delite.single({
    var s = ""
    for (i <- 0 until self.numRows - 1) {
      s = s + self(i).toString + "\n"
    }
    if (self.numRows > 0)
      s + self(self.numRows - 1).toString
    else "[ ]"
  })

  def t(): DenseMatrix[T] = Delite.single({
    val out = DenseMatrix[T](self.numCols, self.numRows)
    for (i <- 0 until self.numCols) {
      for (j <- 0 until self.numRows) {
        out(i, j) = self(j, i)
      }
    }
    out.unsafeImmutable
  })

  def Clone(): DenseMatrix[T] = Delite.map[T, T, DenseMatrix[T]](self, (e => e))

  @mutable def mutable(): DenseMatrix[T] = Delite.single({
    val out = DenseMatrix[T](self.numRows, self.numCols)
    for (i <- 0 until self.numRows) {
      for (j <- 0 until self.numCols) {
        out(i, j) = self(i, j)
      }
    }
    out
  })

  def replicate(__arg0: Int, __arg1: Int): DenseMatrix[T] = Delite.single({
    val out = DenseMatrix[T](__arg0 * self.numRows, __arg1 * self.numCols)
    for (ii <- 0 until __arg0) {
      for (i <- 0 until self.numRows) {
        for (jj <- 0 until __arg1) {
          for (j <- 0 until self.numCols) {
            out(ii * self.numRows + i, jj * self.numCols + j) = self(i, j)
          }
        }
      }
    }
    out.unsafeImmutable
  })

  @write def update(__arg0: Int, __arg1: Int, __arg2: T): Unit = Delite.composite(array_update(densematrix_raw_data(self), densematrix_index(self, __arg0, __arg1), __arg2))

  @write def update(__arg0: Int, __arg1: DenseVector[T]): Unit = Delite.composite(self.updateRow(__arg0, __arg1))

  @write def updateRow(__arg0: Int, __arg1: DenseVector[T]): Unit = Delite.single({
    for (j <- 0 until __arg1.length) {
      self(__arg0, j) = __arg1(j)
    }
  })

  @write def updateCol(__arg0: Int, __arg1: DenseVector[T]): Unit = Delite.single({
    for (i <- 0 until __arg1.length) {
      self(i, __arg0) = __arg1(i)
    }
  })

  def <<(__arg0: DenseVector[T]): DenseMatrix[T] = Delite.single({
    val out = DenseMatrix[T](0, 0)
    out <<= self
    out <<= __arg0
    out.unsafeImmutable
  })

  def <<(__arg0: DenseMatrix[T]): DenseMatrix[T] = Delite.single({
    val out = DenseMatrix[T](0, 0)
    out <<= self
    out <<= __arg0
    out.unsafeImmutable
  })

  def <<|(__arg0: DenseVector[T]): DenseMatrix[T] = Delite.single({
    val out = DenseMatrix[T](0, 0)
    out.insertAllCols(0, self)
    out.insertCol(self.numCols, __arg0)
    out.unsafeImmutable
  })

  def <<|(__arg0: DenseMatrix[T]): DenseMatrix[T] = Delite.single({
    val out = DenseMatrix[T](0, 0)
    out.insertAllCols(0, self)
    out.insertAllCols(self.numCols, __arg0)
    out.unsafeImmutable
  })

  @write def <<=(__arg0: DenseVector[T]): Unit = Delite.composite(self.insertRow(self.numRows, __arg0))

  @write def <<=(__arg0: DenseMatrix[T]): Unit = Delite.composite(self.insertAllRows(self.numRows, __arg0))

  @write def insertRow(pos: Int, y: DenseVector[T]): Unit = Delite.single({
    val idx = pos * self.numCols
    if (self.size == 0) densematrix_set_numcols(self, y.length)
    densematrix_insertspace(self, idx, self.numCols)
    val data = densematrix_raw_data(self)
    for (i <- idx until idx + self.numCols) {
      array_update(data, i, y(i - idx))
    }
    densematrix_set_numrows(self, self.numRows + 1)
  })

  @write def insertAllRows(pos: Int, xs: DenseMatrix[T]): Unit = Delite.single({
    val idx = pos * self.numCols
    if (self.size == 0) densematrix_set_numcols(self, xs.numCols)
    val sz = self.numCols * xs.numRows
    densematrix_insertspace(self, idx, sz)
    val data = densematrix_raw_data(self)
    for (i <- idx until idx + sz) {
      array_update(data, i, densematrix_raw_apply(xs, i - idx))
    }
    densematrix_set_numrows(self, self.numRows + xs.numRows)
  })

  @write def insertCol(pos: Int, y: DenseVector[T]): Unit = Delite.single({
    val newCols = self.numCols + 1
    if (self.size == 0) densematrix_set_numrows(self, y.length)
    val outData = array_empty[T](self.numRows * newCols)
    for (i <- 0 until self.numRows) {
      var col = 0
      for (j <- 0 until newCols) {
        if (j == pos) {
          outData(i * newCols + j) = y(i)
        } else {
          outData(i * newCols + j) = self(i, col)
          col += 1
        }
      }
    }
    densematrix_set_raw_data(self, outData.unsafeImmutable)
    densematrix_set_numcols(self, newCols)
  })

  @write def insertAllCols(pos: Int, xs: DenseMatrix[T]): Unit = Delite.single({
    val newCols = self.numCols + xs.numCols
    if (self.size == 0) densematrix_set_numrows(self, xs.numRows)
    val outData = array_empty[T](self.numRows * newCols)
    for (i <- 0 until self.numRows) {
      var col = 0
      for (j <- 0 until newCols) {
        if (j < pos || j >= pos + xs.numCols) {
          outData(i * newCols + j) = self(i, col)
          col += 1
        } else {
          outData(i * newCols + j) = xs(i, j - pos)
        }
      }
    }
    densematrix_set_raw_data(self, outData.unsafeImmutable)
    densematrix_set_numcols(self, newCols)
  })

  @write def trim(): Unit = Delite.single({
    val data = densematrix_raw_data(self)
    if (self.size < array_length(data)) {
      val d = array_empty[T](self.size)
      array_copy(data, 0, d, 0, self.size)
      densematrix_set_raw_data(self, d.unsafeImmutable)
    }
  })

  @write def removeRow(pos: Int): Unit = Delite.composite(self.removeRows(pos, 1))

  @write def removeCol(pos: Int): Unit = Delite.composite(self.removeCols(pos, 1))

  @write def removeRows(pos: Int, num: Int): Unit = Delite.single({
    val idx = pos * self.numCols
    val len = num * self.numCols
    val data = densematrix_raw_data(self)
    array_copy(data, idx + len, data, idx, self.size - (idx + len))
    densematrix_set_numrows(self, self.numRows - num)
  })

  @write def removeCols(pos: Int, num: Int): Unit = Delite.single({
    val newCols = self.numCols - num
    val outData = array_empty[T](self.numRows * newCols)
    for (i <- 0 until self.numRows) {
      var col = 0
      for (j <- 0 until self.numCols) {
        if (j < pos || j >= pos + num) {
          outData(i * newCols + col) = self(i, j)
          col += 1
        }
      }
    }
    densematrix_set_raw_data(self, outData.unsafeImmutable)
    densematrix_set_numcols(self, newCols)
  })

  def +(__arg0: DenseMatrix[T])(implicit __imp0: Arith[T]): DenseMatrix[T] = Delite.zip[T, T, T, DenseMatrix[T]](self, __arg0, ((a, b) => a + b))

  def +(__arg0: T)(implicit __imp0: Arith[T]): DenseMatrix[T] = Delite.map[T, T, DenseMatrix[T]](self, (e => e + __arg0))

  @write def +=(__arg0: DenseMatrix[T])(implicit __imp0: Arith[T]): Unit = Delite.composite({
    val indices = IndexVector(0, self.size)
    indices.foreach { i => densematrix_raw_update(self, i, densematrix_raw_apply(self, i) + densematrix_raw_apply(__arg0, i)) }
  })

  @write def +=(__arg0: T)(implicit __imp0: Arith[T]): Unit = Delite.composite({
    val indices = IndexVector(0, self.size)
    indices.foreach { i => densematrix_raw_update(self, i, densematrix_raw_apply(self, i) + __arg0) }
  })

  def -(__arg0: DenseMatrix[T])(implicit __imp0: Arith[T]): DenseMatrix[T] = Delite.zip[T, T, T, DenseMatrix[T]](self, __arg0, ((a, b) => a - b))

  def -(__arg0: T)(implicit __imp0: Arith[T]): DenseMatrix[T] = Delite.map[T, T, DenseMatrix[T]](self, (e => e - __arg0))

  @write def -=(__arg0: DenseMatrix[T])(implicit __imp0: Arith[T]): Unit = Delite.composite({
    val indices = IndexVector(0, self.size)
    indices.foreach { i => densematrix_raw_update(self, i, densematrix_raw_apply(self, i) - densematrix_raw_apply(__arg0, i)) }
  })

  @write def -=(__arg0: T)(implicit __imp0: Arith[T]): Unit = Delite.composite({
    val indices = IndexVector(0, self.size)
    indices.foreach { i => densematrix_raw_update(self, i, densematrix_raw_apply(self, i) - __arg0) }
  })

  def *:*(__arg0: DenseMatrix[T])(implicit __imp0: Arith[T]): DenseMatrix[T] = Delite.zip[T, T, T, DenseMatrix[T]](self, __arg0, ((a, b) => a * b))

  def *(__arg0: T)(implicit __imp0: Arith[T]): DenseMatrix[T] = Delite.map[T, T, DenseMatrix[T]](self, (e => e * __arg0))

  @write def *=(__arg0: DenseMatrix[T])(implicit __imp0: Arith[T]): Unit = Delite.composite({
    val indices = IndexVector(0, self.size)
    indices.foreach { i => densematrix_raw_update(self, i, densematrix_raw_apply(self, i) * densematrix_raw_apply(__arg0, i)) }
  })

  @write def *=(__arg0: T)(implicit __imp0: Arith[T]): Unit = Delite.composite({
    val indices = IndexVector(0, self.size)
    indices.foreach { i => densematrix_raw_update(self, i, densematrix_raw_apply(self, i) * __arg0) }
  })

  def *(__arg0: DenseMatrix[T])(implicit __imp0: Arith[T]): DenseMatrix[T] = Delite.single({
    if (self.numCols != __arg0.numRows) fatal("dimension mismatch: matrix multiply")

    val yT = __arg0.t
    val out = DenseMatrix[T](self.numRows, __arg0.numCols)
    for (rowIdx <- 0 until self.numRows) {
      for (i <- 0 until __arg0.numCols) {
        var acc = self(rowIdx, 0) * yT(i, 0)
        for (j <- 1 until yT.numCols) {
          acc += self(rowIdx, j) * yT(i, j)
        }
        out(rowIdx, i) = acc
      }
    }
    out.unsafeImmutable
  })

  def *(__arg0: DenseVector[T])(implicit __imp0: Arith[T]): DenseVector[T] = Delite.single({
    if (self.numCols != __arg0.length || __arg0.isRow) fatal("dimension mismatch: matrix * vector")
    val out = DenseVector[T](self.numRows, false)
    for (rowIdx <- 0 until self.numRows) {
      out(rowIdx) = self(rowIdx) *:* __arg0
    }
    out.unsafeImmutable
  })

  def /(__arg0: DenseMatrix[T])(implicit __imp0: Arith[T]): DenseMatrix[T] = Delite.zip[T, T, T, DenseMatrix[T]](self, __arg0, ((a, b) => a / b))

  def /(__arg0: T)(implicit __imp0: Arith[T]): DenseMatrix[T] = Delite.map[T, T, DenseMatrix[T]](self, (e => e / __arg0))

  @write def /=(__arg0: DenseMatrix[T])(implicit __imp0: Arith[T]): Unit = Delite.composite({
    val indices = IndexVector(0, self.size)
    indices.foreach { i => densematrix_raw_update(self, i, densematrix_raw_apply(self, i) / densematrix_raw_apply(__arg0, i)) }
  })

  @write def /=(__arg0: T)(implicit __imp0: Arith[T]): Unit = Delite.composite({
    val indices = IndexVector(0, self.size)
    indices.foreach { i => densematrix_raw_update(self, i, densematrix_raw_apply(self, i) / __arg0) }
  })

  def sum()(implicit __imp0: Arith[T]): T = Delite.reduce[T](self, (implicitly[Arith[T]].empty), ((a, b) => a + b))

  def mean()(implicit conv: (T) => Double): Double = Delite.composite(self.map(conv).sum / self.size)

  def abs()(implicit __imp0: Arith[T]): DenseMatrix[T] = Delite.map[T, T, DenseMatrix[T]](self, (e => e.abs))

  def exp()(implicit __imp0: Arith[T]): DenseMatrix[T] = Delite.map[T, T, DenseMatrix[T]](self, (e => e.exp))

  def log()(implicit __imp0: Arith[T]): DenseMatrix[T] = Delite.map[T, T, DenseMatrix[T]](self, (e => e.log))

  def sumRows()(implicit __imp0: Arith[T]): DenseVector[T] = Delite.composite(self.mapRowsToVector { row => row.sum })

  def sumCols()(implicit __imp0: Arith[T]): DenseVector[T] = Delite.composite(self.mapColsToVector { col => col.sum })

  def minRows()(implicit __imp0: Ordering[T]): DenseVector[T] = Delite.composite(self.mapRowsToVector { row => row.min })

  def minCols()(implicit __imp0: Ordering[T]): DenseVector[T] = Delite.composite(self.mapColsToVector { col => col.min })

  def maxRows()(implicit __imp0: Ordering[T]): DenseVector[T] = Delite.composite(self.mapRowsToVector { row => row.max })

  def maxCols()(implicit __imp0: Ordering[T]): DenseVector[T] = Delite.composite(self.mapColsToVector { col => col.max })

  def min()(implicit __imp0: Ordering[T]): T = Delite.reduce[T](self, (self(0, 0)), ((a, b) => if (a < b) a else b))

  def max()(implicit __imp0: Ordering[T]): T = Delite.reduce[T](self, (self(0, 0)), ((a, b) => if (a > b) a else b))

  def minIndex()(implicit __imp0: Ordering[T]): Tuple2[Int, Int] = Delite.composite({
    var min = self(0, 0)
    var minRow = 0
    var minCol = 0
    for (i <- 0 until self.numRows) {
      for (j <- 0 until self.numCols) {
        if (self(i, j) < min) {
          min = self(i, j)
          minRow = i
          minCol = j
        }
      }
    }
    (minRow, minCol)
  })

  def maxIndex()(implicit __imp0: Ordering[T]): Tuple2[Int, Int] = Delite.composite({
    var max = self(0, 0)
    var maxRow = 0
    var maxCol = 0
    for (i <- 0 until self.numRows) {
      for (j <- 0 until self.numCols) {
        if (self(i, j) > max) {
          max = self(i, j)
          maxRow = i
          maxCol = j
        }
      }
    }
    (maxRow, maxCol)
  })

  def :>(__arg0: DenseMatrix[T])(implicit __imp0: Ordering[T]): DenseMatrix[Boolean] = Delite.zip[T, T, Boolean, DenseMatrix[Boolean]](self, __arg0, ((a, b) => a > b))

  def :<(__arg0: DenseMatrix[T])(implicit __imp0: Ordering[T]): DenseMatrix[Boolean] = Delite.zip[T, T, Boolean, DenseMatrix[Boolean]](self, __arg0, ((a, b) => a < b))

  def map[R: Manifest](__arg0: (T) => R): DenseMatrix[R] = Delite.map[T, R, DenseMatrix[R]](self, (e => __arg0(e)))

  def mapRowsToVector[R: Manifest](__arg0: (DenseVectorView[T]) => R): DenseVector[R] = Delite.composite(self.rowIndices.map(i => __arg0(self(i))))

  def mapColsToVector[R: Manifest](__arg0: (DenseVectorView[T]) => R): DenseVector[R] = Delite.composite(self.colIndices.map(i => __arg0(self.getCol(i))))

  def zip[B: Manifest, R: Manifest](__arg0: DenseMatrix[B])(__arg1: (T, B) => R): DenseMatrix[R] = Delite.zip[T, B, R, DenseMatrix[R]](self, __arg0, ((a, b) => __arg1(a, b)))

  def foreach(__arg0: (T) => Unit): Unit = Delite.foreach[T](self, (e => __arg0(e)))
}
