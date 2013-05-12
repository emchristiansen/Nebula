package nebula.imageProcessing

import java.io.File

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Prop.{ forAll, propBoolean }
import org.scalacheck.Properties

import javax.imageio.ImageIO

import nebula.imageProcessing._

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
import nebula.util._
import nebula.imageProcessing._
import nebula._

import scala.Array.{ canBuildFrom, fallbackCanBuildFrom }

import org.opencv.features2d.KeyPoint

import java.awt.image.AffineTransformOp.TYPE_BILINEAR

import breeze.linalg._

import org.opencv.features2d.{ DMatch, KeyPoint }

import DenseMatrixUtil._

import TestUtil._

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
class TestRichImage extends FunSuite {
  val url = getClass.getResource("/goldfish_girl.png")
  val image: Image = ImageIO.read(new File(url.getFile))
  val center = KeyPointUtil(image.getWidth.toFloat / 2, image.getHeight.toFloat / 2)

  test("spot check with a small image", InstantTest) {
    val matrix = DenseMatrix.tabulate(3, 3)((y, x) => 10 * (y * 3 + x))
    val image = matrix.toImage

    val subsampled = (for (y <- 0.0 until 2.8 by 0.4) yield {
      for (x <- 0.0 until 2.8 by 0.4) yield image.getSubPixel(x, y).get.gray.head
    }) toMatrix

    val golden = Seq(
      Seq(0, 0, 3, 7, 11, 15, 19),
      Seq(0, 0, 3, 7, 11, 15, 19),
      Seq(9, 9, 12, 16, 20, 24, 28),
      Seq(21, 21, 24, 28, 32, 36, 40),
      Seq(33, 33, 36, 40, 44, 48, 52),
      Seq(45, 45, 48, 52, 56, 60, 64),
      Seq(57, 57, 60, 64, 68, 72, 76)).toMatrix

    asserty(subsampled == golden)
  }

  test("resizing the image with getSubPixel should be the same as resizing " +
    "it with an AffineOp and linear interpolation", InstantTest) {
    val resizeFactor = 2

    val goldenImage: Image = {
      val resizeOp = new AffineTransformOp(
        AffineTransform.getScaleInstance(resizeFactor, resizeFactor),
        AffineTransformOp.TYPE_BILINEAR)
      resizeOp.filter(image, null)
    }

    val estimatedImage: Image = {
      val resized: Image = new BufferedImage(
        resizeFactor * image.getWidth,
        resizeFactor * image.getHeight,
        image.getType)
      for (y <- 0 until resized.getHeight; x <- 0 until resized.getWidth) {
        val pixel = image.getSubPixel(
          x / resizeFactor.toDouble,
          y / resizeFactor.toDouble)
        asserty(pixel.isDefined)
        resized.setPixel(x, y, pixel.get)
      }
      resized
    }

    val difference =
      (goldenImage.toMatrix - estimatedImage.toMatrix)

    //    dumpImage("getSubPixelGolden", goldenImage)
    //    dumpImage("getSubPixelEstimated", estimatedImage)
    //    dumpImage("getSubPixelDifference_shouldBeZeros", difference.toScaledImage)

    //    asserty(goldenImage.toMatrix == estimatedImage.toMatrix)
  }

  ignore("rotateAboutPoint", FastTest, InteractiveTest) {
    for (theta <- 0.0 until 2 * math.Pi by math.Pi / 16) {
      val rotated = image.rotateAboutPoint(Radians(theta), center)
      val patch = rotated.getSubimageCenteredAtPoint(
        center.pt.x,
        center.pt.y,
        20,
        20)
      TestUtil.dumpImage(f"rotated_${theta}%.2f.png", TestUtil.scale10(patch))
    }
  }

  ignore("scaleAboutPoint", FastTest, InteractiveTest) {
    val exponents = 0.2 until 3.0 by 0.05
    val scaleFactors = exponents.map(e => math.pow(2, e))
    for (scaleFactor <- scaleFactors) {
      val scaled = image.scaleAboutPoint(scaleFactor, center)
      val patch = scaled.getSubimageCenteredAtPoint(
        center.pt.x,
        center.pt.y,
        20,
        20)
      TestUtil.dumpImage(f"scaled_${scaleFactor}%.2f.png", TestUtil.scale10(patch))
    }
  }
}

//object CheckRichImage extends Properties("RichImage") {
//  val url = getClass.getResource("/goldfish_girl.jpg")
//  val image = ImageIO.read(new File(url.getFile))
//
//  def monotonicity(x: Double, y: Double, channel: Int): Boolean = {
//    val upperLeft = image.getSubPixel(x.floor, y.floor).get
//    val upperRight = image.getSubPixel(x.ceil, y.floor).get
//    val lowerLeft = image.getSubPixel(x.floor, y.ceil).get
//
//    // TODO: The following two code blocks are nearly identical.
//    val xPixel = image.getSubPixel(x, y.floor).get
//    val xMonotonic = if (xPixel(channel) > upperLeft(channel)) {
//      upperRight(channel) >= xPixel(channel)
//    } else if (xPixel(channel) < upperLeft(channel)) {
//      upperRight(channel) <= xPixel(channel)
//    } else true
//
//    val yPixel = image.getSubPixel(x.floor, y).get
//    val yMonotonic = if (yPixel(channel) > upperLeft(channel)) {
//      lowerLeft(channel) >= yPixel(channel)
//    } else if (yPixel(channel) < upperLeft(channel)) {
//      lowerLeft(channel) <= yPixel(channel)
//    } else true
//
//    xMonotonic && yMonotonic
//  }
//
//  implicit lazy val arbitraryXYChannel: Arbitrary[Tuple3[Double, Double, Int]] = {
//    val xYChannel = for (
//      x <- Gen.choose(0.0, image.getWidth - 1.0);
//      y <- Gen.choose(0.0, image.getHeight - 1.0);
//      channel <- Gen.choose(0, 3)
//    ) yield (x, y, channel)
//    Arbitrary(xYChannel)
//  }
//
//  property("monotonicity") = forAll {
//    xYChannel: Tuple3[Double, Double, Int] =>
//      {
//        val (x, y, channel) = xYChannel
//        monotonicity(x, y, channel)
//      }
//  }
//}