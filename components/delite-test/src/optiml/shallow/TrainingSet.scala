package optiml.shallow.ops

import scala.math.Ordering.Implicits._
import scala.math.Numeric.Implicits._
import ch.epfl.lamp.autolifter.library._
import ch.epfl.lamp.autolifter.annotations._
import ForgeArray._
import ForgeArrayBuffer._
import Numeric._

/**
 * Operations
 */

object TrainingSet {
  def apply[D: Manifest, L: Manifest](__arg0: DenseMatrix[D], __arg1: DenseVector[L]): TrainingSet[D, L] = /*allocates*/ new TrainingSet[D, L](__arg0, __arg1)

}

class TrainingSet[D: Manifest, L: Manifest](___data: DenseMatrix[D], ___labels: DenseVector[L]) { self =>
  var _data = ___data
  var _labels = ___labels

  import TrainingSet._
  def labels(): DenseVector[L] = Forge.getter(self, self._labels)

  def data(): DenseMatrix[D] = Forge.getter(self, self._data)

  def apply(__arg0: Int, __arg1: Int): D = Delite.composite(self.data.apply(__arg0, __arg1))

  def apply(__arg0: Int): DenseVectorView[D] = Delite.composite(self.data.apply(__arg0))

  def numSamples(): Int = Delite.composite(self.data.numRows)

  def numFeatures(): Int = Delite.composite(self.data.numCols)

}

