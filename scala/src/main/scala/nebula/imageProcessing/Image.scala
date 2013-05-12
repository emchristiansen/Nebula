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
import javax.imageio.ImageIO

///////////////////////////////////////////////////////

/**
 * An image which is not null.
 */
case class Image(image: BufferedImage) {
  asserty(image != null)
}

object Image extends ImageRegionOps with ImageFilterOps with ImageGeometryOps {
  implicit class Image2Box(image: Image) extends Box[BufferedImage] {
    override def get = image.image
  }
  
  def read(file: ExistingFile): Image = ImageIO.read(file)
  
  implicit class CanWrite(image: Image) {
    def write(file: File, extensionOption: Option[String]) {      
      val extension = extensionOption match {
        case None => file.getPath.split("""\.""").last
        case Some(extension) => extension
      }
      
      asserty(ImageIO.write(image, extension, file))
    }
  }

  implicit def Image2BufferedImage(image: Image): BufferedImage =
    image.get

  implicit def bufferedImage2Image(image: BufferedImage): Image =
    Image(image)
}