package nebula.imageProcessing

import nebula._
import java.awt.geom.AffineTransform
import java.awt.image.AffineTransformOp
import java.awt.image.BufferedImage
import java.io.File

import scala.Array.canBuildFrom
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION

import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.apache.commons.math3.linear.ArrayRealVector
import org.apache.commons.math3.linear.LUDecomposition
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.linear.RealMatrix
import org.apache.commons.math3.linear.RealVector
import org.apache.commons.math3.linear.SingularValueDecomposition
import org.opencv.features2d.KeyPoint

import nebula.graveyard.Point2D
import nebula._
import nebula.util.MathUtil._
import org.apache.commons.math3.linear._
import java.awt.Rectangle
import java.awt.geom.AffineTransform
import java.awt.image.AffineTransformOp
import java.awt.image.BufferedImage
import java.awt.image.ConvolveOp
import java.awt.image.Kernel

import org.imgscalr.Scalr
import org.opencv.features2d.KeyPoint

///////////////////////////////////////////////////////

trait ImageRegionOps {
  implicit class ImageRegionOps(image: Image) {
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

    // A subpixel version of |BufferedImage.getSubimage|.
    def getSubimage(
      x: Double,
      y: Double,
      width: Int,
      height: Int): Image = {
      requirey(x >= 0)
      requirey(x + width < image.getWidth)
      requirey(y >= 0)
      requirey(y + height < image.getHeight)

      // Crop out enough of the image to just contain the patch.
      val xInt = x.floor.toInt
      val yInt = y.floor.toInt
      val boundingPatch = image.getSubimage(xInt, yInt, width + 1, height + 1)

      // Translate the patch, using subpixel interpolation, so the pixels
      // line up.
      val xResidual = x - xInt
      val yResidual = y - yInt

      val translation = new AffineTransform
      translation.translate(-xResidual, -yResidual)
      val op = new AffineTransformOp(translation, AffineTransformOp.TYPE_BILINEAR)
      val alignedPatch = op.filter(boundingPatch, null)

      // Extract the aligned patch.
      Image(alignedPatch.getSubimage(0, 0, width, height))
    }

    def getSubimageCenteredAtPoint(
      x: Double,
      y: Double,
      xRadius: Int,
      yRadius: Int): Image =
      getSubimage(
        x - xRadius,
        y - yRadius,
        2 * xRadius,
        2 * yRadius)

    def extractPatch(
      patchWidth: Int,
      keyPoint: KeyPoint): Option[Image] = {
      try {
        val x = keyPoint.pt.x - patchWidth / 2.0
        val y = keyPoint.pt.y - patchWidth / 2.0
        Some(getSubimage(x, y, patchWidth, patchWidth))
      } catch {
        case e: IllegalArgumentException => None
      }
    }

    //    def deepCopy(bi: BufferedImage): BufferedImage = {
    //      val newImage = new BufferedImage(bi.getWidth, bi.getHeight, bi.getType)
    //      val graphics = newImage.createGraphics
    //      graphics.drawImage(bi, null, 0, 0)
    //      newImage
    //    }

    def extractROI(roiImg: Image): Image = {
      // after loading up the roi, figure out the crop window size.
      //val hasAlphaChannel = image.getAlphaRaster() != null
      val cols = roiImg.getWidth()
      val rows = roiImg.getHeight()
      var minx = cols + 1;
      var miny = rows + 1;
      var maxx = -1;
      var maxy = -1;
      val maskedImg = new BufferedImage(cols, rows, image.getType())
      for (i_rows <- 0 until rows) {
        for (i_cols <- 0 until cols) {
          // if this pixel is not 100% black, then this is considered
          // to be an on pixel
          if ((roiImg.getRGB(i_cols, i_rows) & 0x00ffffff) != 0) {
            // copy the pixel color value from original image to the masked
            // image
            maskedImg.setRGB(i_cols, i_rows, image.getRGB(i_cols, i_rows))

            // see if this is a new minx, miny
            if (i_cols < minx) {
              minx = i_cols
            }
            if (i_cols > maxx) {
              maxx = i_cols
            }
            if (i_rows < miny) {
              miny = i_rows
            }
            if (i_rows > maxy) {
              maxy = i_rows
            }
          } // is this an on pixel?
          else {
            maskedImg.setRGB(i_cols, i_rows, Pixel.green)
          } // else of is this an on pixel?

        } // loop over cols
      } // loop over rows
      val width = maxx - minx
      val height = maxy - miny

      // crop the image
      val crop_rect = new Rectangle(minx, miny, width, height)

      val overlap = crop_rect.intersection(new Rectangle(image.getWidth(), image.getHeight()))
      val clipped = maskedImg.getSubimage(overlap.x, overlap.y, overlap.width, overlap.height)

      Image(clipped)
    }

    def takePatches(
      patchWidth: Int,
      numPatchesOption: Option[Int]): Stream[Image] = {
      val patchClosures = for (
        row <- 0 to image.getHeight - patchWidth;
        col <- 0 to image.getWidth - patchWidth
      ) yield { () =>
        Image(image.get.getSubimage(col, row, patchWidth, patchWidth))
      }

      numPatchesOption match {
        case None => patchClosures.toStream map (_.apply)
        case Some(numPatches) =>
          val stride = patchClosures.size / numPatches
          val strided = patchClosures.grouped(stride).map(_.head).toStream.take(numPatches)
          asserty(strided.size == numPatches)
          strided.map(_.apply)
      }
    }
  }
}

