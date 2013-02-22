package nebula.util

import nebula.graveyard._
import nebula.util._
import nebula.imageProcessing._
import nebula._

import java.awt.image.BufferedImage
import java.awt.image.BufferedImage._
import java.io.File

import scala.Array.canBuildFrom
import scala.text.{ DocText, Document }
import scala.util.Random

import org.opencv.features2d.KeyPoint
import org.opencv.core._

import nebula._

import breeze.linalg._

import RichImage._
import MathUtil._
import nebula.util._

import reflect._
import scalaz._
import Scalaz._

///////////////////////////////////////////////////////////

object DenseMatrixUtil {
  def matToMatrixDouble(mat: Mat): Option[DenseMatrix[IndexedSeq[Double]]] = {
    if (mat.rows == 0 || mat.cols == 0 || mat.channels == 0) None
    else {
      val matrix = DenseMatrix.zeros[IndexedSeq[Double]](mat.rows, mat.cols)
      for (row <- 0 until mat.rows; column <- 0 until mat.cols) {
        val doubles = mat.get(row, column)
        matrix(row, column) = doubles.toIndexedSeq
      }

      Some(matrix)
    }
  }

  def matToMatrixDoubleSingleChannel(mat: Mat): Option[DenseMatrix[Double]] = {
    // This weird line turns a Boolean to an Option for nice monad behavior.
    for (
      _ <- (mat.channels == 1).option(None);
      matrixDouble <- matToMatrixDouble(mat)
    ) yield {
      matrixDouble mapValues {
        case Seq(x) => x
      }
    }
  }

  //  def matToMatrixComplex(
  //    mat: Mat): Option[DenseMatrix[SpireComplex[Double]]] = {
  //    requirey(mat.cols % 2 == 0)
  //
  //    val doubleMatrixOption = matToMatrixDouble(mat)
  //    for (doubleMatrix <- doubleMatrixOption) yield {
  //      val data = doubleMatrix.t.copy.data
  //      val complexEntries = data.grouped(2) map {
  //        case Array(real, imaginary) => new SpireComplex(real, imaginary)
  //      }
  //      complexEntries.toIndexedSeq.grouped(doubleMatrix.rows).toIndexedSeq.toMatrix
  //    }
  //  }

  def matToMatrixComplex(
    mat: Mat): Option[DenseMatrix[SpireComplex[Double]]] = {
    requirey(mat.channels == 2)

    val doubleMatrixOption = matToMatrixDouble(mat)
    for (doubleMatrix <- doubleMatrixOption) yield {
      doubleMatrix mapValues {
        case Seq(real, imaginary) => new SpireComplex(real, imaginary)
      }
    }
  }

  def matrixComplexToMat(
    matrix: DenseMatrix[SpireComplex[Double]]): Mat = {
    val reals = matrix mapValues (_.real)
    val imaginaries = matrix mapValues (_.imag)
    
    val realMat = matrixDoubleToMat(reals)
    val imaginaryMat = matrixDoubleToMat(imaginaries)
    
    val planes: java.util.List[Mat] = new java.util.ArrayList()
    planes.add(realMat)
    planes.add(imaginaryMat)
    val complex = new Mat
    Core.merge(planes, complex)
    complex
  }
  
  def matrixDoubleToMat(matrix: DenseMatrix[Double]): Mat = {
    val mat = new Mat(matrix.rows, matrix.cols, CvType.CV_64FC1);
    matrix mapPairs {
      case ((row, column), value) => mat.put(row, column, value)
    }
    mat
  }

  implicit class SeqSeqToDenseMatrix[A: ClassTag](seqSeq: Seq[Seq[A]]) {
    def toMatrix: DenseMatrix[A] = {
      val rows = seqSeq.size
      val cols = seqSeq.head.size
      for (row <- seqSeq) asserty(row.size == cols)

      val matrix = new DenseMatrix[A](rows, cols)
      for (i <- 0 until rows; j <- 0 until cols) {
        matrix(i, j) = seqSeq(i)(j)
      }
      matrix
    }
  }

  implicit class DenseMatrixToSeqSeq[A](matrix: DenseMatrix[A]) {
    def toSeqSeq: IndexedSeq[IndexedSeq[A]] =
      for (i <- 0 until matrix.rows) yield {
        for (j <- 0 until matrix.cols) yield matrix(i, j)
      }
  }

  //  implicit def implicitDenseMatrixToSeqSeq[A](matrix: DenseMatrix[A]) = new {
  //    def toSeqSeq: IndexedSeq[IndexedSeq[A]] =
  //      for (i <- 0 until matrix.rows) yield {
  //        for (j <- 0 until matrix.cols) yield matrix(i, j)
  //      }
  //  }

  implicit class DenseMatrixMethods[A: ClassTag](matrix: DenseMatrix[A]) {
    def rollVertical(deltaY: Int): DenseMatrix[A] =
      matrix.mapPairs({
        case ((y, x), value) => matrix((y - deltaY) mod matrix.rows, x)
      })

    def rollHorizontal(deltaX: Int): DenseMatrix[A] =
      matrix.mapPairs({
        case ((y, x), value) => matrix(y, (x - deltaX) mod matrix.cols)
      })

  }

  implicit class DenseMatrixTo[A <% Double](self: DenseMatrix[A]) {
    //    // Simply dumps the Ints into the BufferedImage, with no concern
    //    // as to the resulting colors.
    //    def toImage: BufferedImage = {
    //      val image = new BufferedImage(self.cols, self.rows, TYPE_INT_ARGB)
    //      for (y <- 0 until self.rows; x <- 0 until self.cols) {
    //        image.setRGB(x, y, self(y, x))
    //      }
    //      image
    //    }

    def toImage: BufferedImage = {
      requirey((self mapValues (_.toDouble)).max <= 255)
      requirey((self mapValues (_.toDouble)).min >= 0)
      val image = new BufferedImage(self.cols, self.rows, TYPE_INT_ARGB)
      for (y <- 0 until self.rows; x <- 0 until self.cols) {
        val value = self(y, x).toDouble.round.toInt

        val pixel = Pixel(255, value, value, value)
        image.setRGB(x, y, pixel.argb)
      }
      image
    }
  }

  implicit class DenseMatrixDoubleTo(self: DenseMatrix[Double]) {
    def toScaledImage: BufferedImage = {
      val translated = self - self.min
      val scaled = (translated / translated.max).map(_ * 255).map(_.round.toInt)
      scaled.toImage
    }
  }

  implicit class AddScaled(self: DenseMatrix[Int]) {
    def toScaledImage: BufferedImage = self.map(_.toDouble).toScaledImage
  }

  implicit class BufferedImageToDenseMatrix(self: BufferedImage) {
    def toMatrix: DenseMatrix[Int] = {
      val matrix = new DenseMatrix[Int](self.getHeight, self.getWidth)
      for (y <- 0 until self.getHeight; x <- 0 until self.getWidth) {
        val pixel = self.getPixel(x, y)
        matrix(y, x) = pixel.gray.head
      }
      matrix
    }
  }
}