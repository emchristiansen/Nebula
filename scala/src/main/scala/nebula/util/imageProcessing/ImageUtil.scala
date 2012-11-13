package nebula.util.imageProcessing

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
import org.imgscalr.Scalr

///////////////////////////////////////////////////////////

object ImageUtil {
  import RichImage._

  def scale(
      scaleFactor: Double, 
      image: BufferedImage): Tuple2[Tuple2[Double, Double], BufferedImage] = {
    if (scaleFactor == 1) ((1, 1), image)
    else {
      val scaledWidth = (scaleFactor * image.getWidth).round.toInt
      val scaledHeight = (scaleFactor * image.getHeight).round.toInt

      val scaled = Scalr.resize(
        image,
        Scalr.Method.ULTRA_QUALITY,
        Scalr.Mode.FIT_EXACT,
        scaledWidth,
        scaledHeight);
      
      val realFactorX = scaledWidth.toDouble / image.getWidth
      val realFactorY = scaledHeight.toDouble / image.getHeight
      
      ((realFactorX, realFactorY), scaled)
    }

    //    val transformMatrix = new AffineTransform(
    //      factor, 0, // column 1
    //      0, factor, // column 2
    //      0, 0) // column 3
    //
    //    val transformOp = new AffineTransformOp(transformMatrix, TYPE_BILINEAR);
    //
    //    val scaled = transformOp.filter(image, null)
    //    assert(scaled.getWidth == (factor * image.getWidth).round)
    //    assert(scaled.getHeight == (factor * image.getHeight).round)
    //    scaled
  }

  def boxBlur(boxWidth: Int, image: BufferedImage): BufferedImage = {

    val pixel = image.getSubPixel(0, 1)

    val kernel = {
      val numPixels = boxWidth * boxWidth
      val kernelValues = Array.fill(numPixels)((1.0 / numPixels).toFloat)
      new Kernel(boxWidth, boxWidth, kernelValues)
    }
    val op = new ConvolveOp(kernel, ConvolveOp.EDGE_ZERO_FILL, null)
    op.filter(image, null)
  }

  // A subpixel version of |BufferedImage.getSubimage|.
  def getSubimage(image: BufferedImage,
                  x: Double,
                  y: Double,
                  width: Int,
                  height: Int): BufferedImage = {
    require(x >= 0)
    require(x + width < image.getWidth)
    require(y >= 0)
    require(y + height < image.getHeight)

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
    alignedPatch.getSubimage(0, 0, width, height)
  }

  def extractPatch(image: BufferedImage,
                   patchWidth: Int,
                   keyPoint: KeyPoint): Option[BufferedImage] = {
    try {
      val x = keyPoint.pt.x - patchWidth / 2.0
      val y = keyPoint.pt.y - patchWidth / 2.0
      Some(getSubimage(image, x, y, patchWidth, patchWidth))
    } catch {
      case e: IllegalArgumentException => None
    }
  }

  def deepCopy(bi: BufferedImage): BufferedImage = {
    val newImage = new BufferedImage(bi.getWidth, bi.getHeight, bi.getType)
    val graphics = newImage.createGraphics
    graphics.drawImage(bi, null, 0, 0)
    newImage
  }

  def extractROI(roiImg: BufferedImage, image: BufferedImage): BufferedImage = {
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

    clipped
  }

  def transparentToGreen(image: BufferedImage): BufferedImage = {
    for (
      y <- 0 until image.getHeight;
      x <- 0 until image.getWidth
    ) {
      val pixel = Pixel.getPixel(image, x, y)
      if (pixel.alpha == 0) image.setRGB(x, y, Pixel.green)
    }
    image
  }

  /* A common value according to http://en.wikipedia.org/wiki/Gamma_compression */
  private val gamma = 2.2

  private def powerImage(in: BufferedImage, pwr: Double): BufferedImage = {
    val out = new BufferedImage(in.getWidth, in.getHeight, in.getType)
    for (
      y <- 0 until out.getHeight;
      x <- 0 until out.getWidth
    ) {
      val pixel = Pixel.power(Pixel.getPixel(in, x, y), pwr)
      out.setRGB(x, y, pixel.argb)
    }
    out
  }

  def toRaw(image: BufferedImage): BufferedImage = powerImage(image, gamma)

  def fromRaw(image: BufferedImage): BufferedImage = powerImage(image, 1.0 / gamma)
}




