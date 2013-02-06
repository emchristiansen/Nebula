package nebula.util

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

//val evenInts = for (n <- Gen.choose(-1000, 1000)) yield 2 * n

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
class TestFFT extends FunSuite with GeneratorDrivenPropertyChecks with ShouldMatchers {
  val random = new scala.util.Random(0)

  def genNum[T: Numeric: Choose] = Gen.oneOf(Gen.negNum[T], Gen.posNum[T])

  implicit def genDouble = genNum[Double]
  implicit def genComplex = Gen(_ =>
    Some(Complex(random.nextDouble, random.nextDouble)))
  implicit def genPowerOfTwo = Gen(_ => {
    val power = (random.nextInt % 8).abs
    val size = math.pow(2, power).toInt
    assert(size > 0)
    Some(size)
  })

  def genPowerOfTwoSeq[T: Gen] = {
    def next = {
      val size = genPowerOfTwo.sample.get

      Gen.listOfN(size, implicitly[Gen[T]]).sample.map(_.toIndexedSeq)
    }

    Gen(_ => next)
  }

  def genPowerOfTwoSeqPair[T: Gen] = {
    def next = {
      val size = genPowerOfTwo.sample.get

      def sample = Gen.listOfN(size, implicitly[Gen[T]]).sample.map(_.toIndexedSeq)
      val Some(left) = sample
      val Some(right) = sample
      Some(left, right)
    }

    Gen(_ => next)
  }

  def genPowerOfTwoMatrix[T: Gen: ClassTag] = {
    def next = {
      val rows = genPowerOfTwo.sample.get
      val cols = genPowerOfTwo.sample.get

      for (data <- Gen.listOfN(rows * cols, implicitly[Gen[T]]).sample) yield {
        assert(data.size == rows * cols)

        data.toIndexedSeq.grouped(rows).toIndexedSeq.toMatrix
      }
    }

    Gen(_ => next)
  }

  def genPowerOfTwoMatrixPair[T: Gen: ClassTag] = {
    def next = {
      val rows = genPowerOfTwo.sample.get
      val cols = genPowerOfTwo.sample.get

      def sample = for (data <- Gen.listOfN(rows * cols, implicitly[Gen[T]]).sample) yield {
        assert(data.size == rows * cols)

        data.toIndexedSeq.grouped(rows).toIndexedSeq.toMatrix
      }

      val Some(left) = sample
      val Some(right) = sample
      Some(left, right)
    }

    Gen(_ => next)
  }

  def complexToApache(complex: Complex) =
    new org.apache.commons.math3.complex.Complex(complex.real, complex.imag)

  def apacheToComplex(apache: org.apache.commons.math3.complex.Complex) =
    Complex(apache.getReal, apache.getImaginary)

  test("apache's ifft should be the inverse of fft", FastTest) {
    forAll(genPowerOfTwoSeq[Complex]) { complex =>
      whenever(complex.size > 0) {
        val fourier =
          new FastFourierTransformer(DftNormalization.STANDARD).transform(
            complex.map(complexToApache).toArray,
            TransformType.FORWARD)

        val estimate =
          new FastFourierTransformer(DftNormalization.STANDARD).transform(
            fourier,
            TransformType.INVERSE) map apacheToComplex toIndexedSeq

        assertNear(complex: IndexedSeq[Complex], estimate)
      }
    }
  }

  test("fft should agree with Apache's implementation", FastTest) {
    forAll(genPowerOfTwoSeq[Complex]) { complex =>
      whenever(complex.size > 0 && complex.size < 5) {
        val estimated = FFT.fft(complex)

        val golden =
          new FastFourierTransformer(DftNormalization.STANDARD).transform(
            complex.map(complexToApache).toArray,
            TransformType.FORWARD) map apacheToComplex toIndexedSeq

        assertNear(estimated, golden)
      }
    }
  }

  test("ifft should agree with Apache's implementation", FastTest) {
    forAll(genPowerOfTwoSeq[Complex]) { complex =>
      whenever(complex.size > 0) {
        val estimated = FFT.ifft(complex)

        val golden = new FastFourierTransformer(DftNormalization.STANDARD).transform(
          complex.map(complexToApache).toArray,
          TransformType.INVERSE) map apacheToComplex toIndexedSeq

        assertNear(estimated, golden)
      }
    }
  }

  test("fft and ifft should be inverses", FastTest) {
    forAll(genPowerOfTwoSeq[Complex]) { complex =>
      val forward = FFT.ifft(FFT.fft(complex))
      val backward = FFT.fft(FFT.ifft(complex))

      assertNear(complex, forward)
      assertNear(complex, backward)
    }
  }

  test("abs on Complex values should return the magnitude", FastTest) {
    forAll(genComplex) { complex =>
      val magnitude = math.sqrt(
        math.pow(complex.real, 2) + math.pow(complex.imag, 2))
      assertNear(complex.abs, magnitude)
    }
  }

  def doubleToComplex(double: Double): Complex = Complex(double, 0)

  test("convolveSameSize should work on simple example", FastTest) {
    val left = IndexedSeq[Double](1, 2, 0, 1)
    val right = IndexedSeq[Double](2, 3, 1, 2)
    val convolution = IndexedSeq[Double](
      9,
      8,
      9,
      6)

    assertNear(convolution.map(doubleToComplex),
      FFT.convolveSameSize(
        left.map(doubleToComplex),
        right.map(doubleToComplex)))
  }

  test("convolveSameSize and fft convolution should be the same on simple examples", FastTest) {
    val left = IndexedSeq[Double](1, 2, 0, 1)
    val right = IndexedSeq[Double](2, 3, 1, 2)

    val leftComplex = left.map(doubleToComplex)
    val rightComplex = right.map(doubleToComplex)

    val bruteConvolution = FFT.convolveSameSize(
      leftComplex,
      rightComplex)

    val fftConvolution = {
      val leftFFT = FFT.fft(leftComplex)
      val rightFFT = FFT.fft(rightComplex)

      val dotTimes = leftFFT.zip(rightFFT) map {
        case (l, r) => l * r
      }

      FFT.ifft(dotTimes)
    }

    assertNear(bruteConvolution, fftConvolution)
  }

  test("fft and ifft should satisfy the convolution property", FastTest) {
    forAll(genPowerOfTwoSeqPair[Complex]) {
      case (left, right) =>
        val bruteConvolution = FFT.convolveSameSize(
          left,
          right)

        val fftConvolution = {
          val leftFFT = FFT.fft(left)
          val rightFFT = FFT.fft(right)

          val dotTimes = leftFFT.zip(rightFFT) map {
            case (l, r) => l * r
          }

          FFT.ifft(dotTimes)
        }

        assertNear(bruteConvolution, fftConvolution)
    }
  }

  test("fft2 and ifft2 should be inverses", FastTest) {
    forAll(genPowerOfTwoMatrix[Complex]) { complex =>
      val forward = FFT.ifft2(FFT.fft2(complex))
      val backward = FFT.fft2(FFT.ifft2(complex))

      assertNear(complex, forward)
      assertNear(complex, backward)
    }
  }

  test("correlationFromPreprocessed on simple example", FastTest) {
    val left = IndexedSeq[Double](0, 2, 3, 7)
    val right = IndexedSeq[Double](5, 3, 0, 9)

    val leftComplex = left.map(doubleToComplex)
    val rightComplex = right.map(doubleToComplex)

    val brute = FFT.correlateSameSize(
      leftComplex,
      rightComplex)

    val fft = {
      val leftFFT = FFT.fft(leftComplex)
      val rightFFT = FFT.fft(rightComplex)

      FFT.correlationFromPreprocessed(leftFFT, rightFFT)
    }

    assertNear(brute, fft)
  }

  test("correlationFromPreprocessed on real values", FastTest) {
    forAll(genPowerOfTwoSeqPair[Double]) {
      case (left, right) =>
        val leftComplex = left map doubleToComplex
        val rightComplex = right map doubleToComplex
        
        val brute = FFT.correlateSameSize(
          leftComplex,
          rightComplex)

        val fft = {
          val leftFFT = FFT.fft(leftComplex)
          val rightFFT = FFT.fft(rightComplex)

          FFT.correlationFromPreprocessed(leftFFT, rightFFT)
        }

        assertNear(brute, fft)
    }
  }

  test("fft2 and ifft2 should satisfy the convolution property on a simple example", FastTest) {
    val left = new DenseMatrix(
      2,
      Array[Double](
        1, 2, 0, 1,
        0, 0, 0, 1))

    val right = new DenseMatrix(
      2,
      Array[Double](
        1, 2, 3, 0,
        3, 3, 0, 0))

    val leftComplex = left mapValues doubleToComplex
    val rightComplex = right mapValues doubleToComplex

    val bruteConvolution = FFT.convolveSameSize(leftComplex, rightComplex)

    val fftConvolution = {
      val leftFFT = FFT.fft2(leftComplex)
      val rightFFT = FFT.fft2(rightComplex)

      val dotTimes = DenseMatrix.zeros[Complex](left.rows, left.cols)
      for (row <- 0 until left.rows; column <- 0 until left.cols) {
        dotTimes(row, column) =
          leftFFT(row, column) * rightFFT(row, column)
      }

      FFT.ifft2(dotTimes)
    }

    assertNear(bruteConvolution, fftConvolution)
  }

  test("fft2 and ifft2 should satisfy the convolution property", FastTest) {
    forAll(genPowerOfTwoMatrixPair[Complex]) {
      // Let's keep the matrices small for speed.
      case (left, right) => whenever(left.size <= 64) {
        val bruteConvolution = FFT.convolveSameSize(
          left,
          right)

        val fftConvolution = {
          val leftFFT = FFT.fft2(left)
          val rightFFT = FFT.fft2(right)

          // I wasn't able to get the built in :* operator to work.
          val dotTimes = DenseMatrix.zeros[Complex](left.rows, left.cols)
          for (row <- 0 until left.rows; column <- 0 until left.cols) {
            dotTimes(row, column) =
              leftFFT(row, column) * rightFFT(row, column)
          }

          FFT.ifft2(dotTimes)
        }

        assertNear(bruteConvolution, fftConvolution)
      }
    }
  }
  
  test("correlateSameSize on real valued matrices", FastTest) {
    forAll(genPowerOfTwoMatrixPair[Double]) {
      // Let's keep the matrices small for speed.
      case (left, right) => whenever(left.size <= 64) {
        val leftComplex = left mapValues doubleToComplex
        val rightComplex = right mapValues doubleToComplex
        
        val brute = FFT.correlateSameSize(
          leftComplex,
          rightComplex)

        val fft = {
          val leftFFT = FFT.fft2(leftComplex)
          val rightFFT = FFT.fft2(rightComplex)

          FFT.correlationFromPreprocessed(leftFFT, rightFFT)
        }
        
        assertNear(brute, fft)
      }
    }
  }
}