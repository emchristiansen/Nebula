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
case class Image(image: BufferedImage) extends Box[BufferedImage] {
  assert(image != null)
  
  override def get = image
}

object Image extends ImageRegionOps with ImageFilterOps with ImageGeometryOps { 
  def read(file: ExistingFile): Image = Image(ImageIO.read(file))
  
  def read(file: File): Image = read(ExistingFile(file))
  
  implicit class CanWrite(image: Image) {
    def write(file: File, extensionOption: Option[String]) {      
      val extension = extensionOption match {
        case None => file.getPath.split("""\.""").last
        case Some(extension) => extension
      }
      
      assert(ImageIO.write(image, extension, file))
    }
  }

//  implicit def bufferedImage2Image(image: BufferedImage): Image =
//    Image(image)
}