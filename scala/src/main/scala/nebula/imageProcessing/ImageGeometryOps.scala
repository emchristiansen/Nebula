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

trait ImageGeometryOps {
  implicit class ImageGeometryOps(image: Image) {
    def rotateAboutPoint(
      theta: Radians,
      keyPoint: KeyPoint): Image = {
      if ((theta: Double) == 0) image
      else {
        val rotateOp = new AffineTransformOp(
          AffineTransform.getRotateInstance(theta, keyPoint.pt.x, keyPoint.pt.y),
          AffineTransformOp.TYPE_BICUBIC)
        val rotated: Image = rotateOp.filter(image, null)

        // Just make sure the point really did stay the same.
        val pointBefore = image.getSubPixel(keyPoint.pt.x, keyPoint.pt.y)
        val pointAfter = rotated.getSubPixel(keyPoint.pt.x, keyPoint.pt.y)
        asserty(pointBefore.isDefined)
        asserty(pointAfter.isDefined)
        //        asserty(pointBefore.get.isSimilar(5, pointAfter.get))

        rotated
      }
    }

    def scaleAboutPoint(
      scaleFactor: Double,
      keyPoint: KeyPoint): Image = {
      if (scaleFactor == 1) image
      else {
        val ((scaleFactorX, scaleFactorY), scaledImage) =
          image.scale(scaleFactor)

        val translateOp = new AffineTransformOp(
          AffineTransform.getTranslateInstance(
            keyPoint.pt.x - scaleFactorX * keyPoint.pt.x,
            keyPoint.pt.y - scaleFactorY * keyPoint.pt.y),
          AffineTransformOp.TYPE_BICUBIC)
        val scaled: Image = translateOp.filter(scaledImage, null)

        // Just make sure the point really did stay the same.
        val pointBefore = image.getSubPixel(keyPoint.pt.x, keyPoint.pt.y)
        val pointAfter = scaled.getSubPixel(keyPoint.pt.x, keyPoint.pt.y)
        asserty(pointBefore.isDefined)
        asserty(pointAfter.isDefined)
        if (scaleFactor >= 1) asserty(pointBefore.get.isSimilar(20, pointAfter.get))

        scaled
      }
    }

    def scale(targetSize: (Int, Int)): Image = {
      val (width, height) = targetSize
      requirey(width > 0)
      requirey(height > 0)

      Scalr.resize(
        image,
        Scalr.Method.ULTRA_QUALITY,
        Scalr.Mode.FIT_EXACT,
        width,
        height)
    }

    def scale(
      scaleFactor: Double): Tuple2[Tuple2[Double, Double], Image] = {
      if (scaleFactor == 1) ((1, 1), image)
      else {
        val scaledWidth = (scaleFactor * image.getWidth).round.toInt
        val scaledHeight = (scaleFactor * image.getHeight).round.toInt

        val scaled = image.scale((scaledWidth, scaledHeight))

        val realFactorX = scaledWidth.toDouble / image.getWidth
        val realFactorY = scaledHeight.toDouble / image.getHeight

        ((realFactorX, realFactorY), scaled)
      }
    }
  }
}