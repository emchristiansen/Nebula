package nebula.testing

import nebula._
import nebula.util._
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalatest._
import org.scalatest.prop._

import spire.algebra._
import spire.math._
import spire.implicits._
import breeze.linalg._
import reflect._

////////////////////////

/**
 * ScalaCheck style generators.
 */
object Generators {
  /**
   * Generate an arbitrary number.
   */
  def genNum[T: scala.Numeric: Choose] = Gen.oneOf(Gen.negNum[T], Gen.posNum[T])

  /**
   * Generates an arbitrary complex number.
   */
  def genComplex[T: Fractional: Trig: scala.Numeric: Choose] = Gen { _ =>
    for (
      real <- genNum[T].sample;
      imaginary <- genNum[T].sample
    ) yield Complex(real, imaginary)
  }

  /**
   * Generates a positive power of 2.
   */
  def genPowerOfTwo(maxPower: Int) = Gen { _ =>
    requirey(maxPower >= 0)
    for (
      power <- Gen.choose[Int](0, maxPower).sample
    ) yield 2 ** power
  }

  /**
   * Generates a pair of powers of two with a maximum product.
   */
  def genPowerOfTwoPair(maxProductPower: Int) = Gen { _ =>
    requirey(maxProductPower >= 0)
    for (
      left <- genPowerOfTwo(maxProductPower).sample;
      right <- genPowerOfTwo(maxProductPower - MathUtil.log2Exact(left)).sample
    ) yield (left, right)
  }

  /**
   * Generates a pair of sequences of the same size.
   */
  def genSeqPair[T](maxSize: Int, gen: Gen[T]) = Gen { _ =>
    requirey(maxSize >= 0)
    for (
      size <- Gen.choose[Int](0, maxSize).sample;
      left <- Gen.listOfN(size, gen).sample;
      right <- Gen.listOfN(size, gen).sample
    ) yield (left, right)
  }

  /**
   * Generates a DenseMatrix with a given size.
   */
  def genMatrixNM[T: ClassTag](rows: Int, cols: Int, gen: Gen[T]) = Gen { _ =>
    requirey(rows > 0)
    requirey(cols > 0)
    for (elements <- Gen.listOfN(rows * cols, gen).sample) yield new DenseMatrix(rows, elements.toArray)
  }

  /**
   * Generates a DenseMatrix with a randomly generated size.
   */
  def genMatrix[T: ClassTag](genSize: Gen[(Int, Int)], genElement: Gen[T]) =
    Gen { _ =>
      for (
        (rows, cols) <- genSize.sample;
        matrix <- genMatrixNM(rows, cols, genElement).sample
      ) yield matrix
    }

  /**
   * Generates a DenseMatrix pair where the width and height are randomly
   * generated.
   */
  def genMatrixPair[T: ClassTag](genSize: Gen[(Int, Int)], genElement: Gen[T]) =
    Gen { _ =>
      for (
        (rows, cols) <- genSize.sample;
        left <- genMatrixNM(rows, cols, genElement).sample;
        right <- genMatrixNM(rows, cols, genElement).sample
      ) yield (left, right)
    }

  /**
   * Generates a KeyPoint located somewhere inside the specified rectangle.
   * A buffer can be specified to ensure the point is not near an edge.
   */
  def genKeyPoint(width: Int, height: Int, buffer: Int) = Gen {
    case Params(_, random) =>
      val x = random.nextFloat * (width - 2 * buffer) + buffer
      val y = random.nextFloat * (height - 2 * buffer) + buffer
      // TODO: I've forgotten why this next line is like this.
      Some(KeyPointUtil(x.floor + 0.5.toFloat, y.floor + 0.5.toFloat))
  }
}