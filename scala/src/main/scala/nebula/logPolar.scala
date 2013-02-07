package nebula

import java.awt.image.BufferedImage

import org.opencv.features2d.KeyPoint

import breeze.linalg.{ DenseMatrix, DenseVector, copy }
import nebula.util.DenseMatrixUtil._
import nebula.imageProcessing.ImageUtil
import nebula.imageProcessing.RichImage.bufferedImage
import scala.reflect._

///////////////////////////////////////////////////////////

object LogPolar {
  //  implicit val epsilon = nebula.Epsilon(0.00001)

  // Get the minimum, maximum, and exponential base of the scaling
  // factors. These are used to create the image pyramid from
  // which descriptors are extracted.
  // TODO: Why doesn't this just return the factors directly?
  def getFactors(
    samplingRadius: Double,
    minRadius: Double,
    maxRadius: Double,
    numScales: Int): Tuple3[Double, Double, Double] = {
    val maxScalingFactor = samplingRadius / minRadius
    val minScalingFactor = samplingRadius / maxRadius
    asserty(maxScalingFactor > minScalingFactor)
    val base = math.exp(math.log(minScalingFactor / maxScalingFactor) / (numScales - 1))
    (minScalingFactor, maxScalingFactor, base)
  }

  // Get the image pyramid, as well as the scaling factors that
  // were used to create it. Since the ideal scaling factors cannot be
  // used in all cases (integer rounding), we return the ideal factors
  // plus the actual factors used.
  def scaleImage(
    samplingRadius: Double,
    minRadius: Double,
    maxRadius: Double,
    numScales: Int,
    blurWidth: Int,
    image: BufferedImage): Tuple3[IndexedSeq[Double], IndexedSeq[Tuple2[Double, Double]], IndexedSeq[BufferedImage]] = {
    // We build a set of rescaled images. The biggest image is scaled
    // so that |minRadius| in the original image is |samplingRadius|
    // in the scaled image. The smallest image is scaled so that
    // |maxRadius| maps to |samplingRadius|.

    val (minScalingFactor, maxScalingFactor, base) =
      getFactors(samplingRadius, minRadius, maxRadius, numScales)
    asserty(base < 1)

    val idealScaleFactors = for (scaleIndex <- 0 until numScales) yield {
      val scaleFactor = maxScalingFactor * math.pow(base, scaleIndex)
      asserty(scaleFactor >= minScalingFactor - epsilon)
      asserty(scaleFactor <= maxScalingFactor + epsilon)
      if (scaleIndex == 0) assertNear(scaleFactor, maxScalingFactor)
      if (scaleIndex == numScales - 1) assertNear(scaleFactor, minScalingFactor)
      scaleFactor
    }

    val blurred = ImageUtil.boxBlur(blurWidth, image)

    val (realFactors, scaledImages) = (for (scaleFactor <- idealScaleFactors) yield {
      ImageUtil.scale(scaleFactor, blurred)
    }) unzip

    (idealScaleFactors, realFactors, scaledImages)
  }

  def rawLogPolarSeq(
    normalizeScale: Boolean,
    minRadius: Double,
    maxRadius: Double,
    numScales: Int,
    numAngles: Int,
    blurWidth: Int)(
      image: BufferedImage,
      keyPoints: Seq[KeyPoint]): Seq[Option[DenseMatrix[Int]]] = {
    // The larger this number, the more accurate the sampling
    // but the larger the largest resized image.
    val samplingRadius = 4.0

    val (idealScaleFactors, realScaleFactors, scaledImages) = scaleImage(
      samplingRadius,
      minRadius,
      maxRadius,
      numScales,
      blurWidth,
      image)

    for (keyPoint <- keyPoints) yield {
      def isInsideBounds(keyPoint: KeyPoint): Boolean = {
        val x = keyPoint.pt.x * realScaleFactors.last._1
        val y = keyPoint.pt.y * realScaleFactors.last._2
        val width = scaledImages.last.getWidth
        val height = scaledImages.last.getHeight

        x - epsilon > samplingRadius &&
          x + epsilon + samplingRadius < width &&
          y - epsilon > samplingRadius &&
          y + epsilon + samplingRadius < height
      }

      if (!isInsideBounds(keyPoint)) None
      else {
        val matrix = DenseMatrix.fill(numScales, numAngles)(0)
        for (scaleIndex <- 0 until numScales; angleIndex <- 0 until numAngles) {
          val scaledImage = scaledImages(scaleIndex)
          val (scaleFactorX, scaleFactorY) = realScaleFactors(scaleIndex)

          val (scaledX, scaledY) = (
            scaleFactorX * keyPoint.pt.x,
            scaleFactorY * keyPoint.pt.y)

          val angle = 2 * math.Pi * angleIndex.toDouble / numAngles
          val pixelOffset = DenseVector(
            samplingRadius * math.sin(angle),
            samplingRadius * math.cos(angle))

          val (x, y) = (scaledX + pixelOffset(0), scaledY + pixelOffset(1))
          val pixel = scaledImage.getSubPixel(x, y).get

          matrix(scaleIndex, angleIndex) = pixel.gray.head
        }
        Some(matrix)
      }
    }
  }

  def rawLogPolar(
    normalizeScale: Boolean,
    minRadius: Double,
    maxRadius: Double,
    numScales: Int,
    numAngles: Int,
    blurWidth: Int)(
      image: BufferedImage,
      keyPoint: KeyPoint): Option[DenseMatrix[Int]] = {
    rawLogPolarSeq(
      normalizeScale,
      minRadius,
      maxRadius,
      numScales,
      numAngles,
      blurWidth)(
        image,
        Seq(keyPoint)).head
  }

  //  // Pad with |cols| - 1 columns of zeros on either side, and replicate
  //  // once vertically, dropping the last row.
  //  def prepareMatrixForConvolution(matrix: DenseMatrix[Double]): DenseMatrix[Double] = {
  //    val seqSeq = matrix.toSeqSeq
  //
  //    val zeroPadding = IndexedSeq.fill(matrix.cols - 1)(0.0)
  //    val padded = seqSeq.map(zeroPadding ++ _ ++ zeroPadding)
  //
  //    val replicated = (padded ++ padded.init).toMatrix
  //    asserty(replicated.cols == 3 * matrix.cols - 2)
  //    asserty(replicated.rows == 2 * matrix.rows - 1)
  //    replicated
  //  }

  //  def stackVertical(matrix: DenseMatrix[Double]): DenseMatrix[Double] = {
  //    val seqSeq = matrix.toSeqSeq
  //    (seqSeq ++ seqSeq.init).toMatrix
  //  }

  def getResponseMap[N <% Normalizer[DenseMatrix[Int], DenseMatrix[F2]], M <% Matcher[DenseMatrix[F2]], F2](
    normalizer: N,
    normalizeByOverlap: Boolean,
    matcher: M,
    base: DenseMatrix[Int],
    kernel: DenseMatrix[Int],
    angleIndices: Range,
    scaleIndices: Range)(
      implicit ed: ((N, M)) => ExpectedDistance): DenseMatrix[Double] = {
    requirey(2 * kernel.cols - 1 == base.cols)
    requirey(kernel.rows == base.rows)
    requirey(angleIndices.min >= 0)
    requirey(angleIndices.max < kernel.cols)
    requirey(scaleIndices.min >= -kernel.rows + 1)
    requirey(scaleIndices.max < kernel.rows)

    val response = DenseMatrix.fill(scaleIndices.size, angleIndices.size)(0.0)
    for (scaleIndex <- scaleIndices; angleIndex <- angleIndices) {
      val baseScaleRange =
        math.max(0, scaleIndex) until math.min(base.rows, base.rows + scaleIndex)
      val kernelScaleRange =
        math.max(0, -scaleIndex) until math.min(base.rows, base.rows - scaleIndex)

      val baseMatrixUnnormalized = copy(base(
        baseScaleRange,
        angleIndex until angleIndex + kernel.rows))
      val kernelMatrixUnnormalized = copy(kernel(
        kernelScaleRange,
        ::))

      val baseMatrix = normalizer.normalize(baseMatrixUnnormalized)
      val kernelMatrix = normalizer.normalize(kernelMatrixUnnormalized)

      val scaleOffset = scaleIndex - scaleIndices.min
      val unnormalized = matcher.distance(baseMatrix, kernelMatrix)
      response(scaleOffset, angleIndex) = if (normalizeByOverlap) {
        val size = baseMatrix.rows * baseMatrix.cols
        val expectedDistance = (normalizer, matcher).to[ExpectedDistance].expectedDistance(size)
        unnormalized / expectedDistance
      } else unnormalized
    }
    response
  }

  def getResponseMapWrapper[N <% Normalizer[DenseMatrix[Int], DenseMatrix[F2]], M <% Matcher[DenseMatrix[F2]], F2](
    self: LogPolarMatcher[N, M, F2], left: DenseMatrix[Int], right: DenseMatrix[Int])(
      implicit ed: ((N, M)) => ExpectedDistance) = {
    requirey(left.rows == right.rows)
    requirey(left.cols == right.cols)
    requirey(self.scaleSearchRadius >= 0 && self.scaleSearchRadius < left.rows)

    //        val distance = self.matcher.distance

    val angleIndices = if (self.rotationInvariant) {
      0 until left.cols
    } else 0 until 1

    val scaleIndices = -self.scaleSearchRadius to self.scaleSearchRadius

    // TODO
    LogPolar.getResponseMap(
      self.normalizer,
      self.normalizeByOverlap,
      self.matcher,
      DenseMatrix.horzcat(left, left(::, 0 until left.cols - 1)),
      right,
      angleIndices,
      scaleIndices)
  }

  def distance[N <% Normalizer[DenseMatrix[Int], DenseMatrix[F2]], M <% Matcher[DenseMatrix[F2]], F2](self: LogPolarMatcher[N, M, F2])(
    implicit ed: ((N, M)) => ExpectedDistance): Matcher.DescriptorDistance[DenseMatrix[Int]] = (left: DenseMatrix[Int], right: DenseMatrix[Int]) => {
    val response = getResponseMapWrapper(self, left, right)
    response.min
  }
}