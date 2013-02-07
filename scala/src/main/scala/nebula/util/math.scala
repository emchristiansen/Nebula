package nebula.util

import nebula._
import breeze.linalg._
import breeze.math._
import grizzled.math.stats

///////////////////////////////////////////////////////////

object MathUtil {
  def log2(x: Double) = math.log(x) / math.log(2)
  
  // TODO: Clean this crap up with typeclasses.
  def l2Norm[A : Ring](values: Array[A]): Double = new DenseVector(values).norm(2.0)
  
  def mean[A : Numeric](values: Seq[A]): Double = stats.mean(values: _*)
  
  def normalizeL2[A <% Double](values: Seq[A]): Seq[Double] = {
    val doubles = values map (_.toDouble)
    
    val m = mean(doubles)
    val centered = doubles map (_ - m)
    val norm = l2Norm(centered.toArray)
    centered map (_ / norm)
  }
  
  // The correct implementation of a % b.
  implicit class AddMod(self: Double) {
    def mod(that: Double) =
      if (self >= 0) self % that
      else (that + (self % that)) % that
  }

  // The correct implementation of a % b.
  implicit class AddModInt(self: Int) {
    def mod(that: Int) =
      if (self >= 0) self % that
      else (that + (self % that)) % that
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

  implicit class AddToVector(self: DenseMatrix[Double]) {
    def toVector: DenseVector[Double] = { 
      DenseVector(self.data)
    }
  }

  def dot(left: DenseMatrix[Double], right: DenseMatrix[Double]): Double = {
    requirey(left.rows == right.rows)
    requirey(left.cols == right.cols)

    left.toVector.dot(right.toVector)
  }

  def normalizedDot(left: DenseMatrix[Double], right: DenseMatrix[Double]): Double = {
    dot(left, right) / (norm(left.toVector) * norm(right.toVector) + .000001)
  }

  def crossDistance(
    filterFunction: (IndexedSeq[Double], IndexedSeq[Double]) => Double,
    base: DenseMatrix[Double],
    kernel: DenseMatrix[Double]): DenseMatrix[Double] = {
    val filterRows = (base.rows - kernel.rows + 1) assert (_ > 0)
    val filterColumns = (base.cols - kernel.cols + 1) assert (_ > 0)

    val filter = new DenseMatrix[Double](filterRows, filterColumns)
    for (y <- 0 until filterRows; x <- 0 until filterColumns) {
      val baseRegion = copy(base(y until y + kernel.rows, x until x + kernel.cols))
      filter(y, x) = filterFunction(
        baseRegion.data.toIndexedSeq,
        kernel.data.toIndexedSeq)
    }
    filter
  }

  def crossFilter(
    filterFunction: (DenseMatrix[Double], DenseMatrix[Double]) => Double,
    base: DenseMatrix[Double],
    kernel: DenseMatrix[Double]): DenseMatrix[Double] = {
    val filterRows = (base.rows - kernel.rows + 1) assert (_ > 0)
    val filterColumns = (base.cols - kernel.cols + 1) assert (_ > 0)

    val filter = new DenseMatrix[Double](filterRows, filterColumns)
    for (y <- 0 until filterRows; x <- 0 until filterColumns) {
      val baseRegion = copy(base(y until y + kernel.rows, x until x + kernel.cols))
      filter(y, x) = filterFunction(baseRegion, kernel)
    }
    filter
  }

  def crossCorrelation(
    base: DenseMatrix[Double],
    kernel: DenseMatrix[Double]): DenseMatrix[Double] = crossFilter(
    (left: DenseMatrix[Double], right: DenseMatrix[Double]) =>
      dot(left, right).toDouble,
    base,
    kernel)
}