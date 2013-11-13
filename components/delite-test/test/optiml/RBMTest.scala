package ch.epfl.lamp.optiml

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import reflect.runtime.universe._
import ch.epfl.yinyang.runtime.YYStorage
import optiml.shallow.ops._

@RunWith(classOf[JUnitRunner])
class RBMTest extends FlatSpec with ShouldMatchers {

  "RBM" should "work" in intercept[scala.NotImplementedError] {
    val res = optiML {
      val args = Seq("", "", "")
      val maxEpoch = 10 // maximum number of epochs
      val numHiddenUnits = args(1).toInt

      val epsilonw = 0.1 // Learning rate for weights
      val epsilonvb = 0.1 // Learning rate for biases of visible units
      val epsilonhb = 0.1 // Learning rate for biases of hidden units
      val weightCost = 0.0002
      val initialMomentum = 0.5
      val finalMomentum = 0.9

      // println("Using " + numHiddenUnits + " hidden units.")

      // println("Reading MNIST dataset")
      val numCases = args(2).toInt // batchSize
      val trainingData = readMatrix(args(0))
      val numDims = trainingData.numCols
      val numBatches = trainingData.numRows / numCases

      // tic(trainingData)

      // Initialize symmetric weights and biases
      val visHid = (DenseMatrix.randn(numDims, numHiddenUnits) * 0.1)
      val hidBiases = DenseVector.zeros(numHiddenUnits)
      val visBiases = DenseVector.zeros(numDims)

      val visHidInc = DenseMatrix.zeros(numDims, numHiddenUnits)
      val hidBiasInc = DenseVector.zeros(numHiddenUnits)
      val visBiasInc = DenseVector.zeros(numDims)

      val errSum: Double = 0.0 // needed because all arguments in the tuple must be explicitly Reps
      var epochErrSum = 0.0
      var epoch = 0
      var batch = 0

      implicit def diffRBM(t1: Tuple6[DenseMatrix[Double], DenseMatrix[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double]],
                           t2: Tuple6[DenseMatrix[Double], DenseMatrix[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double]]) = 10.0

      val p = untilconverged_buffered[(DenseMatrix[Double], DenseMatrix[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double])]((visHidInc, visHid, visBiasInc, visBiases, hidBiasInc, hidBiases), maxIter = numBatches * maxEpoch) {
        params =>
          // val (errSum,visHidInc,visHid,visBiasInc,visBiases,hidBiasInc,hidBiases) = t7(params)
          // val (visHidInc, visHid, visBiasInc, visBiases, hidBiasInc, hidBiases) = t6(params)
          val visHidInc = params._1
          val visHid = params._2
          val visBiasInc = params._3
          val visBiases = params._4
          val hidBiasInc = params._5
          val hidBiases = params._6

          //println("Epoch: " + epoch + ", Batch: " + batch)
          // Positive phase
          val data = trainingData.sliceRows(batch * numCases, (batch + 1) * numCases) // data: numCases x numDims
          val posHidProbs = (data * visHid + hidBiases.replicate(numCases, 1)).map(sigmoid)
          val posProds = data.t * posHidProbs
          val posHidAct = posHidProbs.sumCols
          val posVisAct = data.sumCols
          val posHidStates = (posHidProbs :> DenseMatrix.rand(numCases, numHiddenUnits)).map(e => if (e) 1.0 else 0.0)

          // Negative phase
          val negData = (posHidStates * visHid.t + visBiases.replicate(numCases, 1)).map(sigmoid)
          val negHidProbs = (negData * visHid + hidBiases.replicate(numCases, 1)).map(sigmoid)
          val negProds = negData.t * negHidProbs
          val negHidAct = negHidProbs.sumCols
          val negVisAct = negData.sumCols
          val diff = data - negData
          val nextErrSum = (diff *:* diff).sum

          // Update weights and biases
          val momentum = if (epoch > 5) finalMomentum else initialMomentum
          val nextVisHidInc = visHidInc * momentum + ((posProds - negProds) / numCases - (visHid * weightCost)) * epsilonw
          val nextVisHid = visHid + nextVisHidInc
          val nextVisBiasInc = visBiasInc * momentum + (posVisAct - negVisAct) * (epsilonvb / numCases)
          val nextVisBiases = visBiases + nextVisBiasInc
          val nextHidBiasInc = hidBiasInc * momentum + (posHidAct - negHidAct) * (epsilonhb / numCases)
          val nextHidBiases = hidBiases + nextHidBiasInc

          batch += 1
          epochErrSum += nextErrSum

          if (batch == numBatches) {
            batch = 0
            epoch += 1
            // println("--> Epoch " + epoch)
            // println(" error = " + epochErrSum)
            epochErrSum = 0.0
          }

          // (nextErrSum, nextVisHidInc, nextVisHid, nextVisBiasInc, nextVisBiases, nextHidBiasInc, nextHidBiases)
          (nextVisHidInc, nextVisHid, nextVisBiasInc, nextVisBiases, nextHidBiasInc, nextHidBiases)
      }
      p

    }

    // assert(res == ())
  }

}
