package nebula.util.imageProcessing

import java.awt.{Color, Rectangle}
import java.awt.color.ColorSpace
import java.awt.geom.AffineTransform
import java.awt.image.{AffineTransformOp, BufferedImage, ColorConvertOp, ConvolveOp, DataBufferInt, Kernel}

import nebula.graveyard._
import nebula.mpie._
import nebula.summary._
import nebula.smallBaseline._
import nebula.util._
import nebula.util.imageProcessing._
import nebula.wideBaseline._
import nebula._

import scala.Array.{canBuildFrom, fallbackCanBuildFrom}

import org.opencv.features2d.KeyPoint

///////////////////////////////////////////////////////////

// TODO: Make trait
case class RichImage(image: BufferedImage) {
  def getPixel(x: Int, y: Int): Pixel = {
    require(x >= 0 && x < image.getWidth)
    require(y >= 0 && y < image.getHeight)
    Pixel.getPixel(image, x, y)
  }

  // Uses linear interpolation.
  def getSubPixel(x: Double, y: Double): Option[Pixel] = {
    if (!(x >= 0 && x <= image.getWidth - 1) ||
      !(y >= 0 && y <= image.getHeight - 1)) None
    else {
      def intsAndWeights(double: Double): List[Tuple2[Int, Double]] = {
        val floor = double.floor
        if (floor == double) {
          List((double.toInt, 1.0))
        } else {
          val floorWeight = 1 - (double - floor)
          val ceil = double.ceil
          val ceilWeight = 1 - (ceil - double)
          List((floor.toInt, floorWeight), (ceil.toInt, ceilWeight))
        }
      }

      val xIntsAndWeights = intsAndWeights(x)
      val yIntsAndWeights = intsAndWeights(y)

      val summands = for (
        (xInt, xWeight) <- xIntsAndWeights;
        (yInt, yWeight) <- yIntsAndWeights
      ) yield {
        val weight = xWeight * yWeight
        val pixel = getPixel(xInt, yInt)

        val Pixel(a, r, g, b) = pixel
        List(weight * a, weight * r, weight * g, weight * b)
      }

      val List(a, r, g, b) = summands.transpose.map(_.sum)
      Some(Pixel(a.toInt, r.toInt, g.toInt, b.toInt))
    }
  }
}

object RichImage {
  implicit def bufferedImage(image: BufferedImage): RichImage = RichImage(image)
}