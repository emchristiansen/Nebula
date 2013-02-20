package nebula.testing

import nebula._
import nebula.util._
import org.scalatest._
import org.scalatest.prop._
import javax.imageio.ImageIO
import java.io.File
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random
import breeze.linalg._

import org.scalacheck._
import breeze.math._

import org.apache.commons.math3.transform.DftNormalization
import org.apache.commons.math3.transform.FastFourierTransformer
import org.apache.commons.math3.transform.TransformType
import DenseMatrixUtil._
import reflect._

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
class TestGenerators extends FunSuite with GeneratorDrivenPropertyChecks with ShouldMatchers {
  import TestUtil._
  
  test("genNum generates negative and positive numbers", FastTest) {
    forAll(Gen.listOfN(1000, Generators.genNum[Int])) { numbers =>
      asserty(numbers.count(_ < 0) > 0)
      asserty(numbers.count(_ > 0) > 0)
    }
  }
  
  test("genPowerOfTwo", FastTest) {
    forAll(Generators.genPowerOfTwo(10)) { x =>
      asserty(FFT.isPowerOf2(x))
    }
  }
  
  test("genSeqPair generates nontrivial seqs", FastTest) {
    forAll(Gen.listOfN(1000, Generators.genSeqPair(4, Gen.posNum[Int]))) {
      seqPairs => 
        asserty(seqPairs.map(_._1.size).max > 0)
    }
  }
  
  test("genSeqPair generates balanced pairs", FastTest) {
    forAll(Generators.genSeqPair(100, Gen.posNum[Int])) {
      case (left, right) => asserty(left.size == right.size)
    }
  }
  
  test("genPowerOfTwoPair", FastTest) {
    val maxProductPower = 10
    forAll(Generators.genPowerOfTwoPair(maxProductPower)) {
      case (left, right) =>
        asserty(FFT.isPowerOf2(left))
        asserty(FFT.isPowerOf2(right))
        asserty(left * right <= math.pow(2, maxProductPower))
    }
  }
  
  test("genMatrixPair", FastTest) {
    val genSize = Generators.genPowerOfTwoPair(10)
    forAll(Generators.genMatrixPair(genSize, Gen.posNum[Int])) {
      case (left, right) =>
        requirey(left.rows > 0)
        requirey(left.cols > 0)
        requirey(left.rows == right.rows)
        requirey(left.cols == right.cols)
    }
  }
}