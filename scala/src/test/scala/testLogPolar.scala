import org.scalatest.FunSuite
import org.opencv.features2d._
import javax.imageio.ImageIO
import java.io.File
import org.opencv.core.MatOfKeyPoint
import org.opencv.features2d.{ FeatureDetector, KeyPoint }
import nebula._
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

import TestUtil._

import scala.util._
import nebula.util.imageProcessing.RichImage._
import MathUtil._
import grizzled.math.stats

import org.imgscalr.Scalr

///////////////////////////////////////////////////////////

class TestLogPolar extends FunSuite {
  val image = ImageIO.read(new File(
    getClass.getResource("/iSpy.jpg").getFile))

  val random = new Random(0)

  def randomPoint(width: Int, height: Int, buffer: Int): KeyPoint = {
    val x = random.nextFloat * (width - 2 * buffer) + buffer
    val y = random.nextFloat * (height - 2 * buffer) + buffer
    //    KeyPointUtil(x, y)
    KeyPointUtil(x.floor + 0.5.toFloat, y.floor + 0.5.toFloat)
  }

  // |theta| is in radians.
  def rotateAboutPoint(
    theta: Double,
    image: BufferedImage,
    keyPoint: KeyPoint): BufferedImage = {
    if (theta == 0) image
    else {
      val rotateOp = new AffineTransformOp(
        AffineTransform.getRotateInstance(theta, keyPoint.pt.x, keyPoint.pt.y),
        AffineTransformOp.TYPE_BICUBIC)
      val rotated = rotateOp.filter(image, null)

      // Just make sure the point really did stay the same.
      val pointBefore = image.getSubPixel(keyPoint.pt.x, keyPoint.pt.y)
      val pointAfter = rotated.getSubPixel(keyPoint.pt.x, keyPoint.pt.y)
      assert(pointBefore.isDefined)
      assert(pointAfter.isDefined)
      assert(pointBefore.get.isSimilar(5, pointAfter.get))

      rotated
    }
  }

  def scaleAboutPoint(
    scaleFactor: Double,
    image: BufferedImage,
    keyPoint: KeyPoint): BufferedImage = {
    if (scaleFactor == 1) image
    else {
      val translateOp = new AffineTransformOp(
        AffineTransform.getTranslateInstance(
          keyPoint.pt.x - scaleFactor * keyPoint.pt.x,
          keyPoint.pt.y - scaleFactor * keyPoint.pt.y),
        AffineTransformOp.TYPE_BICUBIC)
      val scaled = translateOp.filter(ImageUtil.scale(scaleFactor, image)._2, null)

      // Just make sure the point really did stay the same.
      val pointBefore = image.getSubPixel(keyPoint.pt.x, keyPoint.pt.y)
      val pointAfter = scaled.getSubPixel(keyPoint.pt.x, keyPoint.pt.y)
      assert(pointBefore.isDefined)
      assert(pointAfter.isDefined)
      if (scaleFactor >= 1) assert(pointBefore.get.isSimilar(20, pointAfter.get))

      scaled
    }
  }

  test("rollVertical on small matrix") {
    val matrix = DenseMatrix((1, 2), (3, 4))
    val golden = DenseMatrix((3, 4), (1, 2))
    val estimated = matrix.rollVertical(1)
    assert(golden === estimated)
  }

  val width = image.getWidth
  val height = image.getHeight

  val normalizeScale = false
  val minRadius = 4
  val maxRadius = 16
  val numScales = 4
  val numAngles = 8
  val blurWidth = 5

  ignore("scaleImage should work properly") {
    val (scaleFactors, _, scaledImages) = LogPolar.scaleImage(
      4,
      minRadius,
      maxRadius,
      numScales,
      blurWidth,
      image)

    println(scaleFactors)
    for (i <- 0 until scaledImages.size) {
      dumpImage("rawLogPolarScaleImage_%s".format(i), scaledImages(i))
    }
  }

  ignore("rawLogPolar should do rotation correctly") {
    val numPoints = 1
    val points = numPoints times { randomPoint(width, height, 100) }

    for (point <- points; angleIndex <- 0 until numAngles) {
      //      println(point)
      val angle = 2 * math.Pi * angleIndex.toDouble / numAngles
      //      println(angleIndex, angle)

      def extractor = LogPolar.rawLogPolar(
        normalizeScale,
        minRadius,
        maxRadius,
        numScales,
        numAngles,
        blurWidth) _

      val original = extractor(image, point).get
      val rotated = {
        val rotatedImage = rotateAboutPoint(
          angle,
          image,
          point)
        //        dumpImage("rawLogPolarRotationRotatedImage", rotatedImage)
        extractor(rotatedImage, point).get
      }

      val unrotated = rotated.rollVertical(angleIndex)

      //      println(original)
      //      println("herej")
      //      println(unrotated)
      //      
      //      dumpImage("rawLogPolarRotationOriginal", scale100(original.toScaledImage))
      //      dumpImage("rawLogPolarRotationRotated", scale100(rotated.toScaledImage))
      //      dumpImage("rawLogPolarRotationUnrotated", scale100(unrotated.toScaledImage))

      val diff = (original - unrotated)
      //      println(
      //        diff.map(_.abs).max,
      //        diff.map(_.abs).argmax,
      //        angleIndex,
      //        stats.mean(diff.data: _*))
      //      println(stats.mean(diff.data.map(_.abs): _*))
      assert(stats.mean(diff.data.map(_.abs): _*) < 10)
    }
  }

  ignore("rawLogPolar should do scale correctly") {
    val numPoints = 1
    val points = numPoints times { randomPoint(width, height, 100) }

    val scaleFactors = LogPolar.scaleImage(
      4.0,
      minRadius,
      maxRadius,
      numScales,
      blurWidth,
      image)._1

    //    println(scaleFactors)

    for (point <- points; scaleIndex <- 0 until numScales) {
      val base = LogPolar.getFactors(4.0, minRadius, maxRadius, numScales)._3
      val scaleFactor = math.pow(base, scaleIndex)

      def extractor = LogPolar.rawLogPolar(
        normalizeScale,
        minRadius,
        maxRadius,
        numScales,
        numAngles,
        blurWidth) _

      val original = extractor(image, point).get

      val scaled = extractor(scaleAboutPoint(
        1 / scaleFactor,
        image,
        point),
        point).get

      val overlapOriginal = copy(original(
        ::,
        0 until original.cols - scaleIndex))

      val overlapScaled = copy(scaled(
        ::,
        scaleIndex until original.cols))

      //      dumpImage("rawLogPolarScaleOriginal", scale100(original.toScaledImage))
      //      dumpImage("rawLogPolarScaleScaled", scale100(scaled.toScaledImage))
      //      dumpImage("rawLogPolarScaleOverlapOriginal", scale100(overlapOriginal.toScaledImage))
      //      dumpImage("rawLogPolarScaleOverlapScaled", scale100(overlapScaled.toScaledImage))
      //
      //      println(scaleIndex, scaleFactor, base)
      //      println(overlapOriginal.map(_.toDouble) / overlapOriginal.sum.toDouble)
      //      println("here")
      //      println(overlapScaled.map(_.toDouble) / overlapScaled.sum.toDouble)
      //
      //      val testDiff = overlapOriginal.toScaledImage.toMatrix - overlapScaled.toScaledImage.toMatrix

      //      dumpImage("rawLogPolarScaleDiff", testDiff.toScaledImage)

      //      println(testDiff.map(_.abs).argmax, testDiff.map(_.abs).max)
      //      assert(testDiff.map(_.abs).max < 5)

      //      println((overlapOriginal - overlapScaled).map(_.abs).max)
      val diff = overlapOriginal - overlapScaled
      //      assert((overlapOriginal - overlapScaled).map(_.abs).max < 50)
      //      println(stats.mean(diff.data.map(_.abs): _*))
      assert(stats.mean(diff.data.map(_.abs): _*) < 30)
    }
  }

  test("pixel processing should work with LogPolarExtractor") {
    val numPoints = 4
    val points = numPoints times { randomPoint(width, height, 100) }

    for (point <- points) {
      val nccExtractor = LogPolarExtractor(
        PatchExtractorType.NCC,
        normalizeScale,
        false,
        minRadius,
        maxRadius,
        numScales,
        numAngles,
        blurWidth,
        "Gray")

      {
        val descriptor = nccExtractor.extractSingle(image, point).get
        val data = descriptor.original.asInstanceOf[DenseMatrix[Double]].data
        assertNear2(stats.mean(data: _*), 0)
        assertNear2(stats.sampleStdDev(data: _*), 1)
      }

      val rankExtractor = nccExtractor.copy(extractorType = PatchExtractorType.Rank)

      {
        val descriptor = rankExtractor.extractSingle(image, point).get
        val data = descriptor.original.asInstanceOf[DenseMatrix[Double]].data
        assert(data.min == 0)
        assert(data.max == data.size - 1)
        assert(data.distinct.size == data.size)
      }
    }
  }

  test("per-ring normalization") {
    val numPoints = 1
    val points = numPoints times { randomPoint(width, height, 100) }

    for (point <- points) {
      val extractor = LogPolarExtractor(
        PatchExtractorType.NCC,
        normalizeScale,
        true,
        minRadius,
        maxRadius,
        numScales,
        numAngles,
        blurWidth,
        "Gray")

      val descriptor = extractor.extractSingle(image, point).get
      val matrix = descriptor.original.asInstanceOf[DenseMatrix[Double]]
      for (column <- matrix.toSeqSeq.transpose) {
        assertNear2(stats.mean(column: _*), 0)
        assertNear2(stats.sampleStdDev(column: _*), 1)
      }
    }
  }

  ignore("recover proper angle") {
    val numPoints = 1
    val points = numPoints times { randomPoint(width, height, 100) }

    for (point <- points; angleIndex <- 0 until numAngles) {
      val angle = angleIndex.toDouble / numAngles * 2 * math.Pi

      val extractor = LogPolarExtractor(
        PatchExtractorType.Rank,
        normalizeScale,
        false,
        minRadius,
        maxRadius,
        numScales,
        numAngles,
        blurWidth,
        "Gray")

      val original = extractor.extractSingle(image, point).get.original.asInstanceOf[DenseMatrix[Double]]
      val rotated = extractor.extractSingle(rotateAboutPoint(
        angle,
        image,
        point),
        point).get.original.asInstanceOf[DenseMatrix[Double]]

      def distance = LogPolar.getDistance(MatcherType.L1)

      val response = LogPolar.getResponseMap(
        PatchExtractorType.Raw,
        false,
        distance,
        LogPolar.stackVertical(original),
        rotated,
        0 until numAngles,
        0 until 1)

      assert(response.argmin == (angleIndex, 0))
    }
  }

  test("recover proper scale") {
    val numPoints = 1
    val points = numPoints times { randomPoint(width, height, 100) }

    val scaleIndices = (-numScales + 1) until numScales
    for (point <- points; scaleIndex <- scaleIndices.sortBy(_.abs)) {
      val extractor = LogPolarExtractor(
        PatchExtractorType.Raw,
        normalizeScale,
        false,
        minRadius,
        maxRadius,
        numScales,
        numAngles,
        blurWidth,
        "Gray")

      val original = extractor.extractSingle(
        image,
        point).get.original.asInstanceOf[DenseMatrix[Double]]

      val base = LogPolar.getFactors(4.0, minRadius, maxRadius, numScales)._3
      val scaleFactor = math.pow(base, scaleIndex)

      val scaled = extractor.extractSingle(scaleAboutPoint(
        scaleFactor,
        image,
        point),
        point).get.original.asInstanceOf[DenseMatrix[Double]]

      def distance = LogPolar.getDistance(MatcherType.L1)

      //      dumpImage("rawLogPolarRecoverProperScale_original", scale100(original.toScaledImage))
      //      dumpImage("rawLogPolarRecoverProperScale_scaled", scale100(scaled.toScaledImage))

      val response = LogPolar.getResponseMap(
        PatchExtractorType.Raw,
        true,
        distance,
        LogPolar.stackVertical(original),
        scaled,
        0 until 1,
        scaleIndices)

      //      println(response)
      //      println(scaleIndex)
      //
      //      dumpImage("rawLogPolarRecoverProperScale_response", scale100(response.toScaledImage))
      assert(response.argmin == (0, scaleIndex + numScales - 1))
    }
  }
}