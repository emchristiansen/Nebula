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

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
@WrapWith(classOf[ConfigMapWrapperSuite])
class TestNCCLogPolarMatcher(
  val configMap: Map[String, Any]) extends ConfigMapFunSuite with GeneratorDrivenPropertyChecks with ShouldMatchers {
  import TestUtil._

  test("nccFromUnnormalized on no-op example", FastTest) {
    val unnormalizedInnerProduct = 10
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
      left mapValues (_.toDouble) mapValues TestUtil.doubleToComplex,
      right mapValues (_.toDouble) mapValues TestUtil.doubleToComplex) mapValues TestUtil.complexToDouble

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
        leftNormalized mapValues TestUtil.doubleToComplex,
        rightNormalized mapValues TestUtil.doubleToComplex) mapValues TestUtil.complexToDouble
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
}