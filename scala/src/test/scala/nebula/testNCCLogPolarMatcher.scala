package nebula

import nebula._
import org.scalatest._
import javax.imageio.ImageIO
import java.io.File
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import nebula.util._
import nebula.summary._
import nebula.JsonProtocols._
import spray.json._
import nebula.brown._
import com.sun.xml.internal.bind.v2.model.runtime.RuntimeClassInfo
import breeze.linalg._
import breeze.math._
import grizzled.math.stats
import org.scalacheck._
import org.scalatest.prop._
import org.scalatest._
import DenseMatrixUtil._

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
@WrapWith(classOf[ConfigMapWrapperSuite])
class TestNCCLogPolarMatcher(
  val configMap: Map[String, Any]) extends ConfigMapFunSuite with GeneratorDrivenPropertyChecks with ShouldMatchers {
  import TestUtil._

  test("nccFromUnnormalized on no-op example", FastTest) {
    val unnormalizedInnerProduct = .7
    val leftData = NormalizationData(
      AffinePair(1, 0),
      3,
      5)

    val rightData = NormalizationData(
      AffinePair(1, 0),
      4,
      5)

    val ncc = NCCLogPolarMatcher.nccFromUnnormalized(
      leftData,
      rightData,
      unnormalizedInnerProduct)

    // Normalization shouldn't do anything in this case.
    assertNear(unnormalizedInnerProduct, ncc)
  }

  def correlationHelper(left: DenseMatrix[Int], right: DenseMatrix[Int]) {
    val unnormalizedCorrelation = FFT.correlateSameSize(
      left mapValues (_.toDouble) mapValues MathUtil.doubleToComplex,
      right mapValues (_.toDouble) mapValues MathUtil.doubleToComplex) mapValues MathUtil.complexToDouble

    val leftData = NCCLogPolarExtractor.getNormalizationData(left)
    val rightData = NCCLogPolarExtractor.getNormalizationData(right)
    val postNormalized = unnormalizedCorrelation mapValues { correlation =>
      NCCLogPolarMatcher.nccFromUnnormalized(
        leftData,
        rightData,
        correlation)
    }

    val ncc = {
      val leftNormalized = TestUtil.normalize(left)
      val rightNormalized = TestUtil.normalize(right)
      FFT.correlateSameSize(
        leftNormalized mapValues MathUtil.doubleToComplex,
        rightNormalized mapValues MathUtil.doubleToComplex) mapValues MathUtil.complexToDouble
    }

    assertNear(postNormalized, ncc)
  }

  test("nccFromUnnormalized on simple example", FastTest) {
    val left = new DenseMatrix(
      2,
      Array(1, 1, 2, 3))

    val right = new DenseMatrix(
      2,
      Array(-1, 5, 2, 1))

    correlationHelper(left, right)
  }

  test("nccFromUnnormalized", FastTest) {
    forAll(TestUtil.genPowerOfTwoMatrixPair[Double]) {
      case (left, right) => whenever(left.size > 1 && left.size <= 64) {
        correlationHelper(
          left mapValues (_.toInt),
          right mapValues (_.toInt))
      }
    }
  }

  def responseMapHelper(
    scaleSearchRadius: Int,
    leftSamples: DenseMatrix[Int],
    rightSamples: DenseMatrix[Int]) {
    val nccResponseMap = {
      val leftBlock = NCCLogPolarExtractor.getNCCBlock(leftSamples)
      val rightBlock = NCCLogPolarExtractor.getNCCBlock(rightSamples)
      NCCLogPolarMatcher.getResponseMap(
        scaleSearchRadius,
        leftBlock,
        rightBlock)
    }

    /**
     * Assuming the dot product is between two unit length vectors, find
     * their l2 distance.
     * Divides by sqrt(2) to undo a previous normalization.
     */
    def dotProductToL2DistanceUnnormalized(dotProduct: Double): Double =
      math.sqrt(2 - 2 * dotProduct) / math.sqrt(2)

    val nccDistanceMap =
      nccResponseMap mapValues dotProductToL2DistanceUnnormalized

    val goldenResponseMap = {
      val matcher = LogPolarMatcher(
        PatchNormalizer.NCC,
        Matcher.L2,
        true,
        true,
        scaleSearchRadius)
      LogPolar.getResponseMapWrapper(matcher, leftSamples, rightSamples)
    }

    assertNear(nccDistanceMap, goldenResponseMap)
  }

  test("responseMapHelper on simple example", FastTest) {
    val left = new DenseMatrix(
      2,
      Array(1, 5, 3, 4, 5, -6, 7, -8))

    val right = new DenseMatrix(
      2,
      Array(-1, 2, -3, -4, 5, -2, -7, 1))

//    val correlation = FFT.correlateSameSize(
//        left mapValues (_.toDouble) mapValues MathUtil.doubleToComplex, 
//        right mapValues (_.toDouble) mapValues MathUtil.doubleToComplex)
//    println(correlation)
        
    responseMapHelper(1, left, right)
  }

  test("responseMapHelper", FastTest) {
    forAll(TestUtil.genPowerOfTwoMatrixPair[Double]) {
      case (left, right) => whenever(left.rows > 1 && left.cols > 2 && left.size <= 128) {
        val numScales = left.rows
        
        responseMapHelper(
          numScales - 1,
          left mapValues (_.toInt),
          right mapValues (_.toInt))
      }
    }
  }
}








