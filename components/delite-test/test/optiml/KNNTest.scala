package ch.epfl.lamp.optiml

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import reflect.runtime.universe._
import ch.epfl.yinyang.runtime.YYStorage
import optiml.shallow.ops._

@RunWith(classOf[JUnitRunner])
class KNNTest extends FlatSpec with ShouldMatchers {

  "KNN" should "work" in intercept[scala.NotImplementedError] {
    val res = optiML {
      def createDataSet = {
        val group = DenseMatrix(VarSeq[DenseVector[Double]]((1.0, 1.1), (1.0, 1.0), (0.0, 0.0), (0.0, 0.1)))
        val labels = DenseVector(VarSeq("A", "A", "B", "B"))
        TrainingSet(group, labels)
      }

      def file2TraniningSet(inputFile: String) = {
        val matrixIn = readMatrix[String](inputFile, s => s)
        val data = matrixIn.slice(0, matrixIn.numRows, 0, matrixIn.numCols - 1).map(x => x.toDouble)
        val labels = matrixIn.getCol(matrixIn.numCols - 1)
        TrainingSet(data, labels)
      }

      def file2TraniningSetAutoNorm(inputFile: String) = {
        val matrixIn = readMatrix[String](inputFile, s => s)
        val data = matrixIn.slice(0, matrixIn.numRows, 0, matrixIn.numCols - 1).map(x => x.toDouble)
        val maxVals = data.maxCols
        val minVals = data.minCols
        val ranges = maxVals - minVals
        // TODO
        val data1 = (0 :: data.numRows - 1, *)(i => (data(i) - minVals) / ranges)
        // val data1 = data.mapRows(row => (row - minVals) / ranges)
        // val data1 = matrixIn.slice(0, matrixIn.numRows, 0, matrixIn.numCols - 1).map(x => x.toDouble)
        val labels = matrixIn.getCol(matrixIn.numCols - 1)
        TrainingSet(data1, labels)
      }

      def KNNClassify(data: DenseMatrix[Double], labels: DenseVector[String], inX: DenseVector[Double], k: Int): String = {
        val kIndices = data.mapRowsToVector(row => dist(row, inX, EUC)).sortWithIndex._2.take(k)
        val kLabels = labels(kIndices).groupBy(i => i)
        val maxIndex = kLabels.map(_.length).maxIndex
        kLabels(maxIndex).apply(0) // return label associated with group
      }

      val args = Seq("", "", "")

      val trainingSet = file2TraniningSetAutoNorm(args(0))
      val hoRatio = args(1).toDouble
      // if  (hoRatio < 0.0 || hoRatio > 1.0) printUsage

      val numTestVecs = floor(hoRatio * trainingSet.data.numRows)
      val testData = trainingSet.data.sliceRows(0, numTestVecs)
      val testLabels = trainingSet.labels.slice(0, numTestVecs)
      val trainData = trainingSet.data.sliceRows(numTestVecs, trainingSet.data.numRows - numTestVecs)
      val trainLabels = trainingSet.labels.slice(numTestVecs, trainingSet.data.numRows - numTestVecs)

      val errCount = sum(0, numTestVecs - 1) { i =>
        val classifyLabel = KNNClassify(trainData, trainLabels, testData(i), 3)
        if (testLabels(i) == classifyLabel) {
          0
        } else {
          // println( "the classifier came back with:"+ classifyLabel + " the real answer is:" + testLabels(i))
          1
        }
      }
      errCount
    }

    // assert(res == ())
  }

}
