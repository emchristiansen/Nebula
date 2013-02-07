package nebula.util

import scala.math._
import breeze.math._
import breeze.linalg._
import DenseMatrixUtil._
import org.apache.commons.math3.transform.DftNormalization
import org.apache.commons.math3.transform.FastFourierTransformer
import org.apache.commons.math3.transform.TransformType
import scala.collection.IndexedSeq
import nebula._

///////////////////////////////////////////////////////////

object FFT {
  def complexToApache(complex: Complex) =
    new org.apache.commons.math3.complex.Complex(complex.real, complex.imag)

  def apacheToComplex(apache: org.apache.commons.math3.complex.Complex) =
    Complex(apache.getReal, apache.getImaginary)

  def isPowerOf2(size: Int): Boolean =
    MathUtil.log2(size) == MathUtil.log2(size).round
    
  def isZeroOrPowerOf2(size: Int): Boolean =
    size == 0 || isPowerOf2(size)

  def fftWrapper(
    transform: TransformType)(
      data: IndexedSeq[Complex]): IndexedSeq[Complex] = {
    requirey(isZeroOrPowerOf2(data.size))
    data.size match {
      case 0 => IndexedSeq()
      case _ => new FastFourierTransformer(DftNormalization.STANDARD).transform(
        data map complexToApache toArray,
        transform) map apacheToComplex toIndexedSeq
    }
  }

  def fft = fftWrapper(TransformType.FORWARD) _

  def ifft = fftWrapper(TransformType.INVERSE) _

  def fft2Wrapper(
    transform: TransformType)(
      data: DenseMatrix[Complex]): DenseMatrix[Complex] = {
    requirey(isZeroOrPowerOf2(data.rows))
    requirey(isZeroOrPowerOf2(data.cols))

    val seqSeq = data.toSeqSeq

    val fftHorz = seqSeq.map(fftWrapper(transform))
    val fftFull = fftHorz.transpose.map(fftWrapper(transform)).transpose

    fftFull.toMatrix
  }

  def fft2 = fft2Wrapper(TransformType.FORWARD) _

  def ifft2 = fft2Wrapper(TransformType.INVERSE) _

  /**
   * Performs correlation (not convolution) between two signals, assuming
   * they were originally purely real and the have already been mapped
   * into Fourier space.
   */
  def correlationFromPreprocessed(
    left: IndexedSeq[Complex],
    right: IndexedSeq[Complex]): IndexedSeq[Complex] = {

    val dotTimes = left.map(_.conjugate).zip(right) map {
      case (l, r) => l * r
    }

    ifft(dotTimes)
  }

  def correlationFromPreprocessed(
    left: DenseMatrix[Complex],
    right: DenseMatrix[Complex]): DenseMatrix[Complex] = {

    val leftConjugate = left mapValues (_.conjugate) 

    // I wasn't able to get the built in :* operator to work.
    val dotTimes = DenseMatrix.zeros[Complex](left.rows, left.cols)
    for (row <- 0 until left.rows; column <- 0 until left.cols) {
      dotTimes(row, column) =
        leftConjugate(row, column) * right(row, column)
    }

    ifft2(dotTimes)
  }

  def correlateSameSize(
    left: IndexedSeq[Complex],
    right: IndexedSeq[Complex]): IndexedSeq[Complex] = {
    requirey(left.size == right.size)

    def shiftLeft[A](seq: IndexedSeq[A]): IndexedSeq[A] = {
      seq.tail :+ seq.head
    }

    def shifts =
      Stream.iterate(right)(shiftLeft).take(right.size).toIndexedSeq

    shifts map { seq =>
      (seq.zip(left) map {
        case (l, r) => l * r
      }).sum
    }
  }

  def convolveSameSize(
    left: IndexedSeq[Complex],
    right: IndexedSeq[Complex]): IndexedSeq[Complex] = {
    requirey(left.size == right.size)

    def shiftLeft[A](seq: IndexedSeq[A]): IndexedSeq[A] = {
      seq.tail :+ seq.head
    }

    def leftShifts =
      Stream.iterate(shiftLeft(left))(shiftLeft).take(left.size).toIndexedSeq

    leftShifts map { left =>
      (left.zip(right.reverse) map {
        case (l, r) => l * r
      }).sum
    }
  }

  def correlateSameSize(
    left: DenseMatrix[Complex],
    right: DenseMatrix[Complex]): DenseMatrix[Complex] = {
    requirey(left.size > 0)
    requirey(left.rows == right.rows)
    requirey(left.cols == right.cols)

    def shiftUp(matrix: DenseMatrix[Complex]): DenseMatrix[Complex] =
      matrix mapActivePairs {
        case ((row, column), _) => matrix(
          (row + 1) % matrix.rows,
          column)
      }

    def shiftLeft(matrix: DenseMatrix[Complex]): DenseMatrix[Complex] =
      matrix mapActivePairs {
        case ((row, column), _) => matrix(
          row,
          (column + 1) % matrix.cols)
      }

    def shifts =
      Stream.iterate(right)(shiftUp).map({ matrix =>
        Stream.iterate(matrix)(shiftLeft).take(left.cols)
      }).take(left.rows)

    val streamStream = shifts map (_.map { matrix =>
      (copy(matrix).data.zip(copy(left).data) map {
        case (l, r) => l * r
      }).sum
    })

    val correlation = streamStream.map(_.toIndexedSeq).toIndexedSeq.toMatrix
    asserty(correlation.rows == left.rows)
    asserty(correlation.cols == left.cols)

    correlation
  }

  def convolveSameSize(
    left: DenseMatrix[Complex],
    right: DenseMatrix[Complex]): DenseMatrix[Complex] = {
    requirey(left.size > 0)
    requirey(left.rows == right.rows)
    requirey(left.cols == right.cols)

    val rightFlipped = right mapActivePairs {
      case ((row, column), _) => right(
        right.rows - row - 1,
        right.cols - column - 1)
    }

    def shiftUp(matrix: DenseMatrix[Complex]): DenseMatrix[Complex] =
      matrix mapActivePairs {
        case ((row, column), _) => matrix(
          (row + 1) % matrix.rows,
          column)
      }

    def shiftLeft(matrix: DenseMatrix[Complex]): DenseMatrix[Complex] =
      matrix mapActivePairs {
        case ((row, column), _) => matrix(
          row,
          (column + 1) % matrix.cols)
      }

    def shifts =
      Stream.iterate(shiftUp(left))(shiftUp).map({ matrix =>
        Stream.iterate(shiftLeft(matrix))(shiftLeft).take(left.cols)
      }).take(left.rows)

    val streamStream = shifts map (_.map { matrix =>
      (copy(matrix).data.zip(copy(rightFlipped).data) map {
        case (l, r) => l * r
      }).sum
    })

    val convolution = streamStream.map(_.toIndexedSeq).toIndexedSeq.toMatrix
    asserty(convolution.rows == left.rows)
    asserty(convolution.cols == left.cols)

    convolution
  }

  // Based on http://rosettacode.org/wiki/Fast_Fourier_transform#Scala
  //  def fft(data: IndexedSeq[Complex]): IndexedSeq[Complex] = {
  //    requirey(data.size == 0 || MathUtil.log2(data.size) == MathUtil.log2(data.size).round)
  //    data.size match {
  //      case 0 => IndexedSeq()
  //      case 1 => data
  //      case n => {
  //        val cis: Double => Complex = phi => Complex(cos(phi), sin(phi))
  //
  //        val e = fft(data.zipWithIndex.filter(_._2 % 2 == 0).map(_._1))
  //        val o = fft(data.zipWithIndex.filter(_._2 % 2 != 0).map(_._1))
  //
  //        val unsorted = for (k <- 0 until n / 2) yield IndexedSeq(
  //          (k, e(k) + o(k) * cis(-2 * Pi * k / n)),
  //          (k + n / 2, e(k) - o(k) * cis(-2 * Pi * k / n)))
  //
  //        //        unsorted.flatten.sortBy(_._1).map(_._2 / math.sqrt(data.size))
  //        unsorted.flatten.sortBy(_._1).map(_._2)
  //      }
  //    }
  //  }
}