package nebula.imageProcessing

import nebula._
import java.awt.image.BufferedImage

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION

///////////////////////////////////////////////////////////

// TODO: Make trait
case class RichImage(image: BufferedImage) {
  def getPixel(x: Int, y: Int): Pixel = {
    requirey(x >= 0 && x < image.getWidth)
    requirey(y >= 0 && y < image.getHeight)
    Pixel.getPixel(image, x, y)
  }

  // Uses linear interpolation.
  def getSubPixel(x: Double, y: Double): Option[Pixel] = {
    if (!(x >= 0 && x < image.getWidth) ||
      !(y >= 0 && y < image.getHeight)) None
    else {
      def intsAndWeights(
        double: Double,
        numPixels: Int): List[Tuple2[Int, Double]] = {
        if (double <= 0.5) List((0, 1.0))
        else if (double >= numPixels - 0.5) List((numPixels - 1, 1.0))
        else {
          val shifted = double - 0.5
          val floor = shifted.floor
          val floorWeight = 1 - (shifted - floor)
          val ceil = shifted.ceil
          val ceilWeight = 1 - floorWeight
          asserty(floorWeight + ceilWeight == 1)
          List((floor.toInt, floorWeight), (ceil.toInt, ceilWeight))
        }
      }

      val xIntsAndWeights = intsAndWeights(x, image.getWidth)
      val yIntsAndWeights = intsAndWeights(y, image.getHeight)

      val summands = for (
        (xInt, xWeight) <- xIntsAndWeights;
        (yInt, yWeight) <- yIntsAndWeights
      ) yield {
        val weight = xWeight * yWeight
        if (weight == 0) List(0.0, 0.0, 0.0, 0.0)
        else {
          val pixel = getPixel(xInt, yInt)

          val Pixel(a, r, g, b) = pixel
          List(weight * a, weight * r, weight * g, weight * b)
        }
      }

      val List(a, r, g, b) = summands.transpose.map(_.sum)
      Some(Pixel(a.round.toInt, r.round.toInt, g.round.toInt, b.round.toInt))
    }
  }

  def setPixel(x: Int, y: Int, pixel: Pixel) {
    image.setRGB(x, y, pixel.argb)
  }

  //  def toMatrix: DenseMatrix[Int] = {
  //    val matrix = new DenseMatrix[Int](image.getHeight, image.getWidth)
  //    for (
  //      rowIndex <- 0 until image.getHeight;
  //      columnIndex <- 0 until image.getWidth
  //    ) {
  //      matrix(rowIndex, columnIndex) = getPixel(columnIndex, rowIndex).gray.head
  //    }
  //    matrix
  //  }
}

object RichImage {
  implicit def bufferedImage(image: BufferedImage): RichImage = RichImage(image)
}