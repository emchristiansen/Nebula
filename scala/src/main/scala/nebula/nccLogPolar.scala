package nebula

import java.awt.image.BufferedImage
import breeze.math._
import breeze.linalg._
import org.opencv.core._
import org.opencv.features2d._
import nebula.util._

///////////////////////////////////////////////////////////

case class AffinePair(scale: Double, offset: Double) {
  requirey(scale > 0)
}

case class NormalizationData(
  affinePair: AffinePair,
  // This is the sum of the elements of the normalized vector.
  elementSum: Double,
  size: Int)

case class ScaleMap[A](data: IndexedSeq[A]) {
  requirey(data.size % 2 == 1)
}

object ScaleMap {
  def apply[A](map: Map[Int, A]): ScaleMap[A] = {
    val minIndex = map.keys.min
    val maxIndex = map.keys.max
    requirey(minIndex == -maxIndex)
    requirey(map.keys.toList.sorted == (minIndex to maxIndex))

    val data = map.toList.sortBy(_._1).map(_._2).toIndexedSeq
    ScaleMap(data)
  }
}

case class NCCBlock(
  fourierData: DenseMatrix[Complex],
  scaleMap: ScaleMap[NormalizationData])

case class NCCLogPolarExtractor(extractor: LogPolarExtractor)

object NCCLogPolarExtractor {
  def getAffinePair(descriptor: DenseMatrix[Int]): AffinePair = {
    requirey(descriptor.size > 1)
    
    val data = descriptor.data
    val offset = MathUtil.mean(data)
    val scale = MathUtil.l2Norm(data map (_ - offset))
    asserty(scale > 0)
    AffinePair(scale, offset)
  }

  def getNormalizationData(descriptor: DenseMatrix[Int]): NormalizationData =
    NormalizationData(
      getAffinePair(descriptor),
      MathUtil.normalizeL2(descriptor.data).sum,
      descriptor.size)

  def getScaleMap(
    descriptor: DenseMatrix[Int]): ScaleMap[NormalizationData] = {
    requirey(descriptor.rows > 0)
    requirey(descriptor.cols > 1)
    
    val numScales = descriptor.rows

    val pairs = for (scaleOffset <- (-numScales + 1) to (numScales - 1)) yield {
      val start = math.max(scaleOffset, 0)
      val stop = math.min(numScales, scaleOffset + numScales)

      val roi = descriptor(start until stop, ::)
      (scaleOffset, getNormalizationData(copy(roi)))
    }

    ScaleMap(pairs.toMap)
  }

  def getNCCBlock(samples: DenseMatrix[Int]): NCCBlock = {
    // We require the descriptor width and height each be a power of two.
    require(FFT.isPowerOf2(samples.rows))
    require(FFT.isPowerOf2(samples.cols))
    require(samples.cols > 1)

    val scaleMap = getScaleMap(samples)

    val fourierData = {
      val zeroPadding = DenseMatrix.zeros[Int](
        samples.rows,
        samples.cols)

      val padded = DenseMatrix.vertcat(samples, zeroPadding)

      FFT.fft2(padded mapValues (r => Complex(r, 0)))
    }

    NCCBlock(fourierData, scaleMap)
  }

  implicit class NCCLogPolarExtractor2Extractor(
    self: NCCLogPolarExtractor) extends SingleExtractor[NCCBlock] {
    override def extractSingle = (image: BufferedImage, keyPoint: KeyPoint) => {
      val samplesOption = self.extractor.extractSingle(image, keyPoint)

      for (samples <- samplesOption) yield {
        asserty(samples.rows == self.extractor.numScales)
        asserty(samples.cols == self.extractor.numAngles)

        getNCCBlock(samples)
      }
    }
  }
}

object NCCLogPolarMatcher {
  def nccFromUnnormalized(
    leftData: NormalizationData,
    rightData: NormalizationData,
    unnormalizedInnerProduct: Double): Double = {
    requirey(leftData.size == rightData.size)

    // Suppose we observe the inner product between two vectors
    // (a_x * x + b_x) and (a_y * y + b_y), where x and y are normalized.
    // Note (a_x * x + b_x)^T (a_y * y + b_y) is
    // (a_x * x)^T (a_y * y) + a_y * b_x^T y + a_x * b_y^T x + b_x^T b_y.
    // Thus we can solve for the normalized dot product:
    // x^T y = ((a_x * x)^T (a_y * y) - a_y * b_x^T y - a_x * b_y^T x - b_x^T b_y) / (a_x * a_y).
    val aybxy =
      rightData.affinePair.scale *
        leftData.affinePair.offset *
        rightData.elementSum

    val axbyx =
      leftData.affinePair.scale *
        rightData.affinePair.offset *
        leftData.elementSum

    val bxby = leftData.size *
      leftData.affinePair.offset *
      rightData.affinePair.offset

    val numerator = unnormalizedInnerProduct - aybxy - axbyx - bxby
    val denominator = leftData.affinePair.scale * rightData.affinePair.scale
    asserty(denominator != 0)

    numerator / denominator
  }
}