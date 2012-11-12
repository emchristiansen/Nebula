package nebula
import org.opencv.features2d._
import javax.imageio.ImageIO
import java.io.File
import org.opencv.core.MatOfKeyPoint
import org.opencv.features2d.{ FeatureDetector, KeyPoint }
import nebula._
//import org.apache.xmlgraphics.image.loader.ImageManager
import org.opencv.core.Mat
import java.awt.Color
import java.awt.image.BufferedImage
import org.apache.commons.math3.linear.Array2DRowRealMatrix
import nebula.util.Homography
import nebula.util.OpenCVUtil
import nebula.util.KeyPointUtil

import javax.imageio.ImageIO

import java.awt.{ Color, Rectangle }
import java.awt.color.ColorSpace
import java.awt.geom.AffineTransform
import java.awt.image.{ AffineTransformOp, BufferedImage, ColorConvertOp, ConvolveOp, DataBufferInt, Kernel }

import nebula.graveyard._
import nebula.mpie._
import nebula.summary._
import nebula.smallBaseline._
import nebula.util._
import nebula.util.imageProcessing._
import nebula.wideBaseline._
import nebula._

import scala.Array.{ canBuildFrom, fallbackCanBuildFrom }

import org.opencv.features2d.KeyPoint

import java.awt.image.AffineTransformOp.TYPE_BILINEAR

import breeze.linalg._

import org.opencv.features2d.{ DMatch, KeyPoint }

import DenseMatrixUtil._

import scala.util._
import nebula.util.imageProcessing.RichImage._
import MathUtil._
import grizzled.math.stats

import breeze.linalg._
import nebula.util.imageProcessing.Pixel
import org.opencv.features2d.KeyPoint
import java.awt.image.BufferedImage
import breeze.linalg.DenseVector
import nebula.util.imageProcessing.ImageUtil
import nebula.util.imageProcessing.RichImage._
import nebula.util.imageProcessing._
import nebula.util.DenseMatrixUtil._

import org.imgscalr.Scalr

///////////////////////////////////////////////////////////

object LogPolar {
  val epsilon = 0.00001

  def getFactors(
    samplingRadius: Double,
    minRadius: Double,
    maxRadius: Double,
    numScales: Int): Tuple3[Double, Double, Double] = {
    val maxScalingFactor = samplingRadius / minRadius
    val minScalingFactor = samplingRadius / maxRadius
    assert(maxScalingFactor > minScalingFactor)
    val base = math.exp(math.log(minScalingFactor / maxScalingFactor) / (numScales - 1))
    (minScalingFactor, maxScalingFactor, base)
  }

  def scaleImage(
    samplingRadius: Double,
    minRadius: Double,
    maxRadius: Double,
    numScales: Int,
    blurWidth: Int,
    image: BufferedImage) = {
    // We build a set of rescaled images. The biggest image is scaled
    // so that |minRadius| in the original image is |samplingRadius|
    // in the scaled image. The smallest image is scaled so that
    // |maxRadius| maps to |samplingRadius|.

    val (minScalingFactor, maxScalingFactor, base) =
      getFactors(samplingRadius, minRadius, maxRadius, numScales)
    assert(base < 1)

    val idealScaleFactors = for (scaleIndex <- 0 until numScales) yield {
      val scaleFactor = maxScalingFactor * math.pow(base, scaleIndex)
      assert(scaleFactor >= minScalingFactor - epsilon)
      assert(scaleFactor <= maxScalingFactor + epsilon)
      if (scaleIndex == 0) assertNear2(scaleFactor, maxScalingFactor)
      if (scaleIndex == numScales - 1) assertNear2(scaleFactor, minScalingFactor)
      scaleFactor
    }

    val blurred = ImageUtil.boxBlur(blurWidth, image)

    val scaledImages = for (scaleFactor <- idealScaleFactors) yield {
      ImageUtil.scale(scaleFactor, blurred)
      //      val scaleOp = new AffineTransformOp(
      //        AffineTransform.getScaleInstance(scaleFactor, scaleFactor),
      //        AffineTransformOp.TYPE_BICUBIC)
      //      scaleOp.filter(blurred, null)
    }

    (idealScaleFactors, scaledImages)
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

    val (scaleFactors, scaledImages) = scaleImage(
      samplingRadius,
      minRadius,
      maxRadius,
      numScales,
      blurWidth,
      image)

    for (keyPoint <- keyPoints) yield {
      def isInsideBounds(keyPoint: KeyPoint): Boolean = {
        val x = keyPoint.pt.x * scaleFactors.last
        val y = keyPoint.pt.y * scaleFactors.last
        val width = scaledImages.last.getWidth
        val height = scaledImages.last.getHeight

        x - epsilon > samplingRadius &&
          x + epsilon + samplingRadius < width &&
          y - epsilon > samplingRadius &&
          y + epsilon + samplingRadius < height
      }

      if (!isInsideBounds(keyPoint)) None
      else {
        val matrix = DenseMatrix.fill(numAngles, numScales)(0)
        for (scaleIndex <- 0 until numScales; angleIndex <- 0 until numAngles) {
          val scaledImage = scaledImages(scaleIndex)

          // The image may not have been scaled precisely according to the
          // scale factor, in order to get an integer size. Here we get
          // the actual scale factors.
          val scaleFactorX = scaledImage.getWidth.toDouble / image.getWidth
          val scaleFactorY = scaledImage.getHeight.toDouble / image.getHeight

          val (scaledX, scaledY) = (
            scaleFactorX * keyPoint.pt.x,
            scaleFactorY * keyPoint.pt.y)
          //          println(scaleFactors(scaleIndex), scaleFactorX, scaleFactorY)

          val angle = 2 * math.Pi * angleIndex.toDouble / numAngles
          val pixelOffset = DenseVector(
            samplingRadius * math.sin(angle),
            samplingRadius * math.cos(angle))
//          println(pixelOffset)

          val (x, y) = (scaledX + pixelOffset(0), scaledY + pixelOffset(1))
          val pixel = scaledImage.getSubPixel(x, y).get
          //          val pixel = scaledImage.getPixel(x.round.toInt, y.round.toInt)

          matrix(angleIndex, scaleIndex) = pixel.gray.head
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

  // Pad with |cols| - 1 columns of zeros on either side, and replicate
  // once vertically, dropping the last row.
  def prepareMatrixForConvolution(matrix: DenseMatrix[Double]): DenseMatrix[Double] = {
    val seqSeq = matrix.toSeqSeq

    val zeroPadding = IndexedSeq.fill(matrix.cols - 1)(0.0)
    val padded = seqSeq.map(zeroPadding ++ _ ++ zeroPadding)

    val replicated = (padded ++ padded.init).toMatrix
    assert(replicated.cols == 3 * matrix.cols - 2)
    assert(replicated.rows == 2 * matrix.rows - 1)
    replicated
  }

  def stackVertical(matrix: DenseMatrix[Double]): DenseMatrix[Double] = {
    val seqSeq = matrix.toSeqSeq
    (seqSeq ++ seqSeq.init).toMatrix
  }

  def getResponseMap(
    normalizeByOverlap: Boolean,
    distance: (IndexedSeq[Double], IndexedSeq[Double]) => Double,
    base: DenseMatrix[Double],
    kernel: DenseMatrix[Double],
    angleIndices: Range,
    scaleIndices: Range): DenseMatrix[Double] = {
    require(2 * kernel.rows - 1 == base.rows)
    require(kernel.cols == base.cols)
    require(angleIndices.min >= 0)
    require(angleIndices.max < kernel.rows)
    require(scaleIndices.min >= -kernel.cols + 1)
    require(scaleIndices.max < kernel.cols)

    val response = DenseMatrix.fill(angleIndices.size, scaleIndices.size)(0.0)
    for (angleIndex <- angleIndices; scaleIndex <- scaleIndices) {
      val baseScaleRange =
        math.max(0, scaleIndex) until math.min(base.cols, base.cols + scaleIndex)
      val kernelScaleRange =
        math.max(0, -scaleIndex) until math.min(base.cols, base.cols - scaleIndex)

      val baseVector = copy(base(
        angleIndex until angleIndex + kernel.rows,
        baseScaleRange)).data.toIndexedSeq
      val kernelVector = copy(kernel(
        ::,
        kernelScaleRange)).data.toIndexedSeq
      assert(baseVector.size == kernelVector.size)
      
      val scaleOffset = scaleIndex - scaleIndices.min
      val unnormalized = distance(baseVector, kernelVector)
      response(angleIndex, scaleOffset) = if (normalizeByOverlap) {
        val denominator = kernel.cols - scaleIndex.abs
        unnormalized / denominator
      } else unnormalized
    }
    response
  }

  //  def getResponseMap(
  //    normalizeByOverlap: Boolean,
  //    distance: (IndexedSeq[Double], IndexedSeq[Double]) => Double,
  //    base: DenseMatrix[Double],
  //    kernel: DenseMatrix[Double],
  //    angleIndices: Range,
  //    scaleIndices: Range): DenseMatrix[Double] = {
  //    require(2 * kernel.rows - 1 == base.rows)
  //    require(3 * kernel.cols - 2 == base.cols)
  //    require(angleIndices.min >= 0)
  //    require(angleIndices.max < kernel.rows)
  //    require(scaleIndices.min >= -kernel.cols + 1)
  //    require(scaleIndices.max < kernel.cols)
  //
  //    val response = DenseMatrix.fill(angleIndices.size, scaleIndices.size)(0.0)
  //    for (angleIndex <- angleIndices; scaleIndex <- scaleIndices) {
  //      val scaleOffset = scaleIndex + kernel.cols - 1
  ////      println(scaleIndices, scaleOffset)
  //      val baseVector = copy(base(
  //        angleIndex until angleIndex + kernel.rows,
  //        scaleOffset until scaleOffset + kernel.cols)).data.toIndexedSeq
  //      val kernelVector = kernel.data.toIndexedSeq
  //      assert(baseVector.size == kernelVector.size)
  //      response(angleIndex, scaleOffset) = distance(baseVector, kernelVector)
  //    }
  //    response
  //  }

  def getDistance(matcherType: MatcherType.MatcherType) = {
    matcherType match {
      case MatcherType.L1 => Matcher.l1[Double] _
      case MatcherType.L2 => Matcher.l2[Double] _
      case _ => sys.error("Not using supported distance")
    }
  }
}

//  def rawLogPolar(
//    normalizeScale: Boolean,
//    minRadius: Double,
//    maxRadius: Double,
//    numScales: Int,
//    numAngles: Int,
//    blurWidth: Int)(
//      image: BufferedImage,
//      keyPoint: KeyPoint): Option[DenseMatrix[Int]] = {
//    assert(!normalizeScale)
//
//    val blurred = ImageUtil.boxBlur(blurWidth, image)
//
//    def pixel(offset: DenseVector[Double]): Pixel = {
//      val (x, y) = (keyPoint.pt.x + offset(0), keyPoint.pt.y + offset(1))
//      blurred.getSubPixel(x, y).get
//    }
//
//    try {
//      // TODO: Make parameter
//      val oversampleFactor = 8
//      val numOversampledAngles = oversampleFactor * numAngles
//      val numOversampledScales = oversampleFactor * numScales
//
//      def pixelOffset(scaleIndex: Int, angleIndex: Int): DenseVector[Double] = {
//        assert(scaleIndex < numOversampledScales)
//        assert(angleIndex < numOversampledAngles)
//
//        val angle = 2 * math.Pi * angleIndex.toDouble / numOversampledAngles
//        val scale = {
//          val base = math.exp(
//            math.log(maxRadius / minRadius) / (numOversampledScales - 1))
//          val scale = minRadius * math.pow(base, scaleIndex)
//          assert(scale >= minRadius, "scale, minRadius: %s, %s".format(scale, minRadius))
//          assert(scale <= maxRadius + .0001, "scale, maxRadius: %s, %s".format(scale, maxRadius))
//          if (scaleIndex == 0) assert(scale == minRadius)
//          if (scaleIndex == numOversampledScales - 1) assertNear(0.0001, scale, maxRadius)
//          scale
//        }
//        DenseVector(scale * math.sin(angle), scale * math.cos(angle))
//      }
//
//      val oversampled = {
//        val image = new BufferedImage(
//          numOversampledScales,
//          numOversampledAngles,
//          BufferedImage.TYPE_BYTE_GRAY)
//        for (
//          angleIndex <- 0 until numOversampledAngles;
//          scaleIndex <- 0 until numOversampledScales
//        ) {
//          image.setPixel(
//            scaleIndex,
//            angleIndex,
//            pixel(pixelOffset(scaleIndex, angleIndex)))
//        }
//        image
//      }
//
//      val scaledImage = ImageUtil.scale(1.0 / oversampleFactor, oversampled)
//      assert(scaledImage.getWidth == numScales)
//      assert(scaledImage.getHeight == numAngles)
//      Some(scaledImage.toMatrix)
//    } catch {
//      case _: NoSuchElementException => None
//    }
//  }