package nebula

import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.io.File

import nebula.imageProcessing._
import nebula.util.DenseMatrixUtil._
import nebula._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._
import org.scalacheck._
import reflect._
import breeze.math._
import breeze.linalg._
import nebula.util._

///////////////////////////////////////////////////////////

/**
 * Less than 0.1s.
 */
object InstantTest extends Tag("nebula.InstantTest")

/**
 * Less than 1s.
 */
object FastTest extends Tag("nebula.FastTest")

/**
 * Less than 10s.
 */
object MediumTest extends Tag("nebula.MediumTest")

/**
 * Less than 100s.
 */
object SlowTest extends Tag("nebula.SlowTest")

/**
 * Requires the user to do something (not fully automatic).
 */
object InteractiveTest extends Tag("nebula.InteractiveTest")

/**
 * External dataset required to run test.
 */
object DatasetTest extends Tag("nebula.DatasetTest")

///////////////////////////////////////////////////////////

trait ConfigMapFunSuite extends FunSuite {
  val configMap: Map[String, Any]

  def datasetRoot: File = {
    require(
      configMap.contains("datasetRoot"),
      "This suite requires a path to external datasets to be passed in the configMap")

    val path = configMap("datasetRoot").asInstanceOf[String]
    new File(path).mustExist
  }
}

object TestUtil {
  System.loadLibrary("opencv_java")

  val random = new scala.util.Random(0)

  def dumpImage(name: String, image: BufferedImage) {
    val tempDirectory = File.createTempFile("temp", "").getParentFile
    asserty(tempDirectory != null)
    val testDirectory = new File(tempDirectory, "nebulaTest")
    println(testDirectory)
    if (!testDirectory.isDirectory) testDirectory.mkdir

    val file = new File(testDirectory, "%s.png".format(name))
    println("writing %s".format(file))
    println("image is size %s by %s".format(image.getWidth, image.getHeight))
    assert(ImageIO.write(image, "png", file))
  }

  def scale10 = (image: BufferedImage) => ImageUtil.scale(10, image)._2
  def scale100 = (image: BufferedImage) => ImageUtil.scale(100, image)._2

//  val runtimeConfig = RuntimeConfig(
//    homeDirectory + "Bitcasa/data",
//    new File("/tmp"),
//    None,
//    false,
//    false)

  def genNum[T: Numeric: Choose] = Gen.oneOf(Gen.negNum[T], Gen.posNum[T])

  implicit def genDouble = genNum[Double]
  implicit def genComplex = Gen(_ =>
    Some(Complex(random.nextDouble, random.nextDouble)))
  def genPowerOfTwo = Gen(_ => {
    val power = (random.nextInt % 8).abs
    val size = math.pow(2, power).toInt
    asserty(size > 0)
    Some(size)
  })

  def genPowerOfTwoSeq[T: Gen] = {
    def next = {
      val size = genPowerOfTwo.sample.get

      Gen.listOfN(size, implicitly[Gen[T]]).sample.map(_.toIndexedSeq)
    }

    Gen(_ => next)
  }

  def genPowerOfTwoSeqPair[T: Gen] = {
    def next = {
      val size = genPowerOfTwo.sample.get

      def sample = Gen.listOfN(size, implicitly[Gen[T]]).sample.map(_.toIndexedSeq)
      val Some(left) = sample
      val Some(right) = sample
      Some(left, right)
    }

    Gen(_ => next)
  }

  def genPowerOfTwoMatrix[T: Gen: ClassTag] = {
    def next = {
      val rows = genPowerOfTwo.sample.get
      val cols = genPowerOfTwo.sample.get

      for (data <- Gen.listOfN(rows * cols, implicitly[Gen[T]]).sample) yield {
        asserty(data.size == rows * cols)

        data.toIndexedSeq.grouped(rows).toIndexedSeq.toMatrix
      }
    }

    Gen(_ => next)
  }

  def genPowerOfTwoMatrixPair[T: Gen: ClassTag] = {
    def next = {
      val rows = genPowerOfTwo.sample.get
      val cols = genPowerOfTwo.sample.get

      def sample = for (data <- Gen.listOfN(rows * cols, implicitly[Gen[T]]).sample) yield {
        asserty(data.size == rows * cols)

        data.toIndexedSeq.grouped(rows).toIndexedSeq.toMatrix
      }

      val Some(left) = sample
      val Some(right) = sample
      Some(left, right)
    }

    Gen(_ => next)
  }

  def normalize(matrix: DenseMatrix[Int]): DenseMatrix[Double] = {
    val mean = MathUtil.mean(matrix.data)
    val centered = matrix mapValues (_ - mean)
    val norm = MathUtil.l2Norm(centered.data)
    centered mapValues (_ / norm)
  }
}