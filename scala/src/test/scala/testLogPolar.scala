import org.scalatest.FunSuite
import org.opencv.features2d._
import javax.imageio.ImageIO
import java.io.File
import org.opencv.core.MatOfKeyPoint
import org.opencv.features2d.{ FeatureDetector, KeyPoint }
import nebula._
import org.apache.xmlgraphics.image.loader.ImageManager
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

///////////////////////////////////////////////////////////

class TestLogPolar extends FunSuite {
  val image = ImageIO.read(new File(
    getClass.getResource("/beer.jpg").getFile))

  val random = new Random(0)

  def randomPoint(width: Int, height: Int, buffer: Int): KeyPoint = {
    val x = random.nextFloat * (width - 2 * buffer) + buffer
    val y = random.nextFloat * (height - 2 * buffer) + buffer
    KeyPointUtil(x, y)
  }

  // |theta| is in radians.
  def rotateAboutPoint(
    theta: Double,
    image: BufferedImage,
    keyPoint: KeyPoint): BufferedImage = {
    val rotateOp = new AffineTransformOp(
      AffineTransform.getRotateInstance(theta, keyPoint.pt.x, keyPoint.pt.y),
      AffineTransformOp.TYPE_BILINEAR)
    val rotated = rotateOp.filter(image, null)

    // Just make sure the point really did stay the same.
    val pointBefore = image.getSubPixel(keyPoint.pt.x, keyPoint.pt.y)
    val pointAfter = rotated.getSubPixel(keyPoint.pt.x, keyPoint.pt.y)
    assert(pointBefore.isDefined)
    assert(pointAfter.isDefined)
    assert(pointBefore.get.isSimilar(5, pointAfter.get))

    rotated
  }

  def scaleAboutPoint(
    scaleFactor: Double,
    image: BufferedImage,
    keyPoint: KeyPoint): BufferedImage = {
    val scaleOp = new AffineTransformOp(
      AffineTransform.getScaleInstance(scaleFactor, scaleFactor),
      AffineTransformOp.TYPE_BILINEAR)
    val translateOp = new AffineTransformOp(
      AffineTransform.getTranslateInstance(
        keyPoint.pt.x - scaleFactor * keyPoint.pt.x,
        keyPoint.pt.y - scaleFactor * keyPoint.pt.y),
      AffineTransformOp.TYPE_BILINEAR)
    val scaled = translateOp.filter(scaleOp.filter(image, null), null)

    // Just make sure the point really did stay the same.
    val pointBefore = image.getSubPixel(keyPoint.pt.x, keyPoint.pt.y)
    val pointAfter = scaled.getSubPixel(keyPoint.pt.x, keyPoint.pt.y)
    assert(pointBefore.isDefined)
    assert(pointAfter.isDefined)
    assert(pointBefore.get.isSimilar(5, pointAfter.get))

    scaled
  }

  ignore("rollVertical on small matrix") {
    val matrix = DenseMatrix((1, 2), (3, 4))
    val golden = DenseMatrix((3, 4), (1, 2))
    val estimated = matrix.rollVertical(1)
    assert(golden === estimated)
  }

  val width = image.getWidth
  val height = image.getHeight

  val normalizeScale = false
  val minRadius = 2
  val maxRadius = 100
  val numScales = 10
  val numAngles = 8
  val blurWidth = 5

  test("rawLogPolar should do rotation correctly") {
    val numPoints = 1
    val points = numPoints times { randomPoint(width, height, 200) }

    for (point <- points; angleIndex <- 0 until numAngles) {
      val angle = angleIndex.toDouble / numAngles * 2 * math.Pi

      def extractor = LogPolar.rawLogPolar(
        normalizeScale,
        minRadius,
        maxRadius,
        numScales,
        numAngles,
        blurWidth) _

      val original = extractor(image, point).get
      val rotated = extractor(rotateAboutPoint(
        angle,
        image,
        point),
        point).get

      val unrotated = rotated.rollVertical(angleIndex)

      //      dumpImage("rawLogPolarRotationOriginal", original.toScaledImage)
      //      dumpImage("rawLogPolarRotationRotated", rotated.toScaledImage)
      //      dumpImage("rawLogPolarRotationUnrotated", unrotated.toScaledImage)

      assert((original - unrotated).map(_.abs).max < 5)
    }
  }

  test("rawLogPolar should to scale correctly") {
    val numPoints = 4
    val points = numPoints times { randomPoint(width, height, 200) }

    for (point <- points; scaleIndex <- 0 until numScales) {
      val base = math.exp(
        math.log(maxRadius / minRadius) / (numScales - 1))
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
        scaleFactor,
        image,
        point),
        point).get

      val overlapOriginal = copy(original(
        ::,
        0 until original.cols - scaleIndex))

      val overlapScaled = copy(scaled(
        ::,
        scaleIndex until original.cols))

//      dumpImage("rawLogPolarScaleOriginal", original.toScaledImage)
//      dumpImage("rawLogPolarScaleScaled", scaled.toScaledImage)
      dumpImage("rawLogPolarScaleOverlapOriginal", overlapOriginal.toImage)
      dumpImage("rawLogPolarScaleOverlapScaled", overlapScaled.toImage)
      
      println(scaleIndex, scaleFactor, base)
      println(overlapOriginal.map(_.toDouble) / overlapOriginal.sum.toDouble)
      println("here")
      println(overlapScaled.map(_.toDouble) / overlapScaled.sum.toDouble)

      val testDiff = overlapOriginal.toScaledImage.toMatrix - overlapScaled.toScaledImage.toMatrix 
      
//      dumpImage("rawLogPolarScaleDiff", testDiff.toScaledImage)
      
//      println(testDiff.map(_.abs).argmax, testDiff.map(_.abs).max)
//      assert(testDiff.map(_.abs).max < 5)
      
      println((overlapOriginal - overlapScaled).map(_.abs).max)
      assert((overlapOriginal - overlapScaled).map(_.abs).max < 50)
    }
  }

  ignore("pixel processing should work with LogPolarExtractor") {
    val numPoints = 4
    val points = numPoints times { randomPoint(width, height, 200) }

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
    val points = numPoints times { randomPoint(width, height, 200) }

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
    val numPoints = 4
    val points = numPoints times { randomPoint(width, height, 200) }

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
        false,
        distance,
        LogPolar.prepareMatrixForConvolution(original),
        rotated,
        0 until numAngles,
        0 until 1)

      assert(response.argmin == (angleIndex, 0))
    }
  }
}