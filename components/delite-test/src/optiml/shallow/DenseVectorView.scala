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

object DenseVectorView {
  def apply[T: Manifest](__arg0: ForgeArray[T], __arg1: Int, __arg2: Int, __arg3: Int, __arg4: Boolean): DenseVectorView[T] = /*allocates*/ new DenseVectorView[T](__arg0, __arg1, __arg2, __arg3, __arg4)

  protected[ops] def densevectorview_data[T: Manifest](self: DenseVectorView[T]): ForgeArray[T] = Forge.getter(self, self._data)

  protected[ops] def densevectorview_start[T: Manifest](self: DenseVectorView[T]): Int = Forge.getter(self, self._start)

  protected[ops] def densevectorview_stride[T: Manifest](self: DenseVectorView[T]): Int = Forge.getter(self, self._stride)

  @simple protected[ops] def densevectorview_illegalalloc[T: Manifest](self: DenseVectorView[T], __arg1: Int): Nothing = Delite.composite(fatal("DenseVectorViews cannot be allocated from a parallel op"))

  @simple protected[ops] def densevectorview_illegalupdate[T: Manifest](self: DenseVectorView[T], __arg1: Int, __arg2: T): Nothing = Delite.composite(fatal("DenseVectorViews cannot be updated"))

  protected[ops] def densevectorview_densevector_filter_map[T: Manifest, R: Manifest](self: DenseVectorView[T], __arg1: (T) => Boolean, __arg2: (T) => R): DenseVector[R] = Delite.filter[T, R, DenseVector[R]](self, (e => __arg1(e)), (e => __arg2(e)))

}

class DenseVectorView[T](___data: ForgeArray[T], ___start: Int, ___stride: Int, ___length: Int, ___isRow: Boolean)(implicit val man: Manifest[T]) extends Vector[T] with ParallelCollection[T] { self =>
  var _data = ___data
  var _start = ___start
  var _stride = ___stride
  var _length = ___length
  var _isRow = ___isRow

  import DenseVectorView._
  def length(): Int = Forge.getter(self, self._length)

  def isRow(): Boolean = Forge.getter(self, self._isRow)

  def apply(__arg0: Int): T = Delite.composite(array_apply(densevectorview_data(self), densevectorview_start(self) + __arg0 * densevectorview_stride(self)))

  def slice(start: Int, end: Int): DenseVectorView[T] = Delite.composite(DenseVectorView(densevectorview_data(self), densevectorview_start(self) + start * densevectorview_stride(self), densevectorview_stride(self), end - start, self.isRow))

  def toDense(): DenseVector[T] = Delite.composite(self.map(e => e))

  def drop(__arg0: Int): DenseVectorView[T] = Delite.composite(self.slice(__arg0, self.length))

  def take(__arg0: Int): DenseVectorView[T] = Delite.composite(self.slice(0, __arg0))

  def count(__arg0: (T) => Boolean): Int = Delite.composite((densevectorview_densevector_filter_map(self, __arg0, (e: T) => 1)).sum)

}
