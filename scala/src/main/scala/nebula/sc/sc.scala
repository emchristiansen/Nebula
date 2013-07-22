package nebula.sc

import nebula._
import breeze.linalg._
import java.awt.image.BufferedImage

///////////////////////////////////////////////

object SC {
  def matchingPursuit(
    x: DenseVector[Double],
    D: DenseMatrix[Double]): DenseVector[Double] = {
    def matchingPursuitIteration(s: DenseVector[Double]) = {
      val estimate = D * s
      val error = x - estimate
      val atomScores: Seq[Double] = (0 until D.cols) map { atomIndex =>
        D(::, atomIndex) dot error
      }
      val (bestDot, bestAtomIndex) = atomScores.zipWithIndex.maxBy(_._1)

      val sCopy = s.copy
      sCopy(bestAtomIndex) += bestDot
      sCopy
    }

    val s = DenseVector.zeros[Double](x.size)
    val iterations = Stream.iterate(s) { s =>
      matchingPursuitIteration(s)
    }

    // The number of atoms per observation.
    iterations.take(5).last
  }

  /**
   * Infers the sources S using matching pursuit.
   */
  def matchingPursuit(
    X: DenseMatrix[Double],
    D: DenseMatrix[Double]): DenseMatrix[Double] = {
    val numObservations = X.cols
    val dimension = X.rows
    val numAtoms = D.cols
    assert(D.rows == dimension)

    for (atomIndex <- 0 until numAtoms) {
      assertNear(norm(D(::, atomIndex)), 1)
    }

    val sources = for (observationIndex <- 0 until numObservations) yield {
      matchingPursuit(X(::, observationIndex), D)
    }

    sources map {
      vector => new DenseMatrix(vector.size, vector.toArray)
    } reduce {
      (left, right) => DenseMatrix.horzcat(left, right)
    }
  }

  def normalizeDictionary(D: DenseMatrix[Double]): DenseMatrix[Double] = {
    val normalized = DenseMatrix.zeros[Double](D.rows, D.cols)
    for (index <- 0 until D.cols) {
      normalized(::, index) := D(::, index) / norm(D(::, index))
    }
    normalized
  }
  
  /**
   * Infers D.
   */
  def inferDictionary(
    X: DenseMatrix[Double],
    S: DenseMatrix[Double]): DenseMatrix[Double] = {
    assert(X.cols == S.cols)

    normalizeDictionary(X \ S)
  }
  
  def randomDictionary(dimension: Int, numAtoms: Int): DenseMatrix[Double] = {
    val random: DenseMatrix[Double] = DenseMatrix.rand(dimension, numAtoms)
    normalizeDictionary(random)
  }
}





