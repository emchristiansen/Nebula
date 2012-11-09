import java.io.File

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Prop.{ forAll, propBoolean }
import org.scalacheck.Properties

import javax.imageio.ImageIO

import nebula.util.imageProcessing.ImageUtil._

import nebula.util.imageProcessing._
import nebula.util.imageProcessing.RichImage._

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

import breeze.linalg.DenseMatrix

import org.opencv.features2d.{ DMatch, KeyPoint }

import DenseMatrixUtil._

import TestUtil._

///////////////////////////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
class TestRichImage extends FunSuite {
  val url = getClass.getResource("/goldfish_girl.jpg")
  val image = ImageIO.read(new File(url.getFile))  
  
  test("resizing the image with getSubPixel should be the same as resizing " +
    "it with an AffineOp and linear interpolation") {
    val resizeFactor = 2
    
    val goldenImage = {
      val resizeOp = new AffineTransformOp(
          AffineTransform.getScaleInstance(resizeFactor, resizeFactor),
          AffineTransformOp.TYPE_BILINEAR)
      resizeOp.filter(image, null)
    }
    
    val estimatedImage = {
      val resized = new BufferedImage(
          resizeFactor * image.getWidth,
          resizeFactor * image.getHeight,
          image.getType)
      for (y <- 0 until resized.getHeight; x <- 0 until resized.getWidth) {
        val pixel = image.getSubPixel(
            x / resizeFactor.toDouble, 
            y / resizeFactor.toDouble)
        assert(pixel.isDefined)
        resized.setPixel(x, y, pixel.get)
      }
      resized
    }

    val difference = 
      (goldenImage.toMatrix - estimatedImage.toMatrix)
      
    dumpImage("getSubPixelGolden", goldenImage)
    dumpImage("getSubPixelEstimated", estimatedImage)
    dumpImage("getSubPixelDifference_shouldBeZeros", difference.toScaledImage)
    
//    assert(goldenImage.toMatrix == estimatedImage.toMatrix)
  }
}

object CheckRichImage extends Properties("RichImage") {
  val url = getClass.getResource("/goldfish_girl.jpg")
  val image = ImageIO.read(new File(url.getFile))

  def monotonicity(x: Double, y: Double, channel: Int): Boolean = {
    val upperLeft = image.getSubPixel(x.floor, y.floor).get
    val upperRight = image.getSubPixel(x.ceil, y.floor).get
    val lowerLeft = image.getSubPixel(x.floor, y.ceil).get

    // TODO: The following two code blocks are nearly identical.
    val xPixel = image.getSubPixel(x, y.floor).get
    val xMonotonic = if (xPixel(channel) > upperLeft(channel)) {
      upperRight(channel) >= xPixel(channel)
    } else if (xPixel(channel) < upperLeft(channel)) {
      upperRight(channel) <= xPixel(channel)
    } else true

    val yPixel = image.getSubPixel(x.floor, y).get
    val yMonotonic = if (yPixel(channel) > upperLeft(channel)) {
      lowerLeft(channel) >= yPixel(channel)
    } else if (yPixel(channel) < upperLeft(channel)) {
      lowerLeft(channel) <= yPixel(channel)
    } else true

    xMonotonic && yMonotonic
  }

  implicit lazy val arbitraryXYChannel: Arbitrary[Tuple3[Double, Double, Int]] = {
    val xYChannel = for (
      x <- Gen.choose(0.0, image.getWidth - 1.0);
      y <- Gen.choose(0.0, image.getHeight - 1.0);
      channel <- Gen.choose(0, 3)
    ) yield (x, y, channel)
    Arbitrary(xYChannel)
  }

  property("monotonicity") = forAll {
    xYChannel: Tuple3[Double, Double, Int] =>
      {
        val (x, y, channel) = xYChannel
        monotonicity(x, y, channel)
      }
  }
}