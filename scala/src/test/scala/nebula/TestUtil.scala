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
    assert(tempDirectory != null)
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

  val runtimeConfig = RuntimeConfig(
    homeDirectory + "Bitcasa/data",
    new File("/tmp"),
    None,
    false,
    false)
}