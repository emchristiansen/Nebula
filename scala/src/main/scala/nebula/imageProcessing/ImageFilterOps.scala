package nebula.imageProcessing

import java.awt.image.BufferedImage

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

trait ImageFilterOps {
  implicit class ImageFilterOps(image: Image) {
    def boxBlur(boxWidth: Int): Image = {
      val pixel = image.getSubPixel(0, 1)

      val kernel = {
        val numPixels = boxWidth * boxWidth
        val kernelValues = Array.fill(numPixels)((1.0 / numPixels).toFloat)
        new Kernel(boxWidth, boxWidth, kernelValues)
      }
      val op = new ConvolveOp(kernel, ConvolveOp.EDGE_ZERO_FILL, null)

      Image(op.filter(image, null))
    }

    def transparentToGreen: Image = {
      for (
        y <- 0 until image.getHeight;
        x <- 0 until image.getWidth
      ) {
        val pixel = Pixel.getPixel(image, x, y)
        if (pixel.alpha == 0) image.setRGB(x, y, Pixel.green)
      }
      image
    }

    private def powerImage(in: Image, pwr: Double): Image = {
      val out = new BufferedImage(in.getWidth, in.getHeight, in.getType)
      for (
        y <- 0 until out.getHeight;
        x <- 0 until out.getWidth
      ) {
        val pixel = Pixel.power(Pixel.getPixel(in, x, y), pwr)
        out.setRGB(x, y, pixel.argb)
      }
      Image(out)
    }

    // A common value according to 
    // http://en.wikipedia.org/wiki/Gamma_compression
    val gamma = 2.2

    def toRaw: Image = powerImage(image, gamma)

    def fromRaw: Image = powerImage(image, 1.0 / gamma)
  }
}