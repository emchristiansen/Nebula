package nebula.util

import breeze.linalg.DenseMatrix

///////////////////////////////////////////////////////////

object MathUtil {
  implicit def implicitSeqSeqToDenseMatrix[A: ClassManifest](seqSeq: IndexedSeq[IndexedSeq[A]]) = new {
    def toMatrix: DenseMatrix[A] = {
      val rows = seqSeq.size
      val cols = seqSeq.head.size
      for (row <- seqSeq) assert(row.size == cols)

      val matrix = new DenseMatrix[A](rows, cols)
      for (i <- 0 until rows; j <- 0 until cols) {
        matrix(i, j) = seqSeq(i)(j)
      }
      matrix
    }
  }

  implicit def implicitDenseMatrixToSeqSeq[A](matrix: DenseMatrix[A]) = new {
    def toSeqSeq: IndexedSeq[IndexedSeq[A]] =
      for (i <- 0 until matrix.rows) yield {
        for (j <- 0 until matrix.cols) yield matrix(i, j)
      }
  }

  // The correct implementation of a % b.
  implicit def addMod(self: Double) = new {
    def mod(that: Double) = self - (self / that).floor * that
  }

  def gaussianKernel(std: Double): Seq[Seq[Double]] = {
    val width = (4 * std).ceil.toInt

    def density(pixel: Int): Double = {
      val normalizer = 1 / (std * math.sqrt(2 * math.Pi))

      val middle = (width - 1).toDouble / 2
      val unnormalizedDensity = math.exp(-math.pow(pixel - middle, 2) / (2 * math.pow(std, 2)))

      normalizer * unnormalizedDensity
    }

    // Isotropic Gaussians decompose.
    val projection = (for (i <- 0 until width) yield density(i)) toList

    for (p1 <- projection) yield {
      for (p2 <- projection) yield {
        p1 * p2
      }
    }
  }
}