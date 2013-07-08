//package nebula.testing
//
//import javax.imageio.ImageIO
//import java.awt.image.BufferedImage
//import java.io.File
//
//import nebula.imageProcessing._
//import nebula.util.DenseMatrixUtil._
//import nebula._
//import org.junit.runner.RunWith
//import org.scalatest.junit.JUnitRunner
//
//import org.scalatest._
//import org.scalacheck._
//import reflect._
//import breeze.math._
//import breeze.linalg._
//import nebula.util._
//import org.scalatest._
//import org.scalatest.prop._
//import org.scalacheck._
//
/////////////////////////////////////////////////////////////
//
///**
// * Less than 0.1s.
// */
//object InstantTest extends Tag("nebula.InstantTest")
//
///**
// * Less than 1s.
// */
//object FastTest extends Tag("nebula.FastTest")
//
///**
// * Less than 10s.
// */
//object MediumTest extends Tag("nebula.MediumTest")
//
///**
// * Less than 100s.
// */
//object SlowTest extends Tag("nebula.SlowTest")
//
///**
// * Requires the user to inspect or do something (not fully automatic).
// */
//object InteractiveTest extends Tag("nebula.InteractiveTest")
//
///**
// * External dataset required to run test.
// */
//object DatasetTest extends Tag("nebula.DatasetTest")
//
/////////////////////////////////////////////////////////////
//
//trait ConfigMapFunSuite extends FunSuite {
//  val configMap: Map[String, Any]
//  // TODO: Remove reflection.
//  def name: String = getClass.getName.replace(".", "_")
//
//  def datasetRoot: ExistingDirectory = {
//    require(
//      configMap.contains("datasetRoot"),
//      "This suite requires a path to external datasets to be passed in the configMap")
//
//    val path = configMap("datasetRoot").asInstanceOf[String]
//    ExistingDirectory(path)
//  }
//
//  def logRoot: ExistingDirectory = {
//    if (!configMap.contains("logRoot")) {
//      val tmp = File.createTempFile("temp", "").getParentFile
//      val logRoot = tmp + "nebulaTestLogRoot"
//      if (!logRoot.isDirectory) logRoot.mkdir
//      ExistingDirectory(logRoot)
//    } else {
//      val path = configMap("logRoot").asInstanceOf[String]
//      ExistingDirectory(path)
//    }
//  }
//
//  def logRootTest: ExistingDirectory = {
//    val directory = (logRoot: File) + name
//    if (!directory.isDirectory) directory.mkdir
//    ExistingDirectory(directory)
//  }
//
//  implicit class TestUtilImageOps(image: Image) {
//    def dumpImage(name: String) {
//      val file: File = (logRootTest: File) + s"$name.png"
//      println(s"Writing ${file.getPath}")
//      println(s"Image is size ${image.getWidth} by ${image.getHeight}")
//      image.write(file, None)
//    }
//
//    def scale10 = image.scale(10)._2
//    def scale100 = image.scale(100)._2
//  }
//}
//
//trait GeneratorFunSuite extends FunSuite with GeneratorDrivenPropertyChecks with ShouldMatchers
//
///**
// * A ScalaTest FunSuite with some useful mixins.
// * Requires the @WrapWith(classOf[ConfigMapWrapperSuite]) annotation and that
// * configMap be initialized.
// */
//trait StandardSuite extends ConfigMapFunSuite with GeneratorFunSuite with TestUtil
//
//trait MySuite extends fixture.FunSuite with GeneratorDrivenPropertyChecks with ShouldMatchers
//
//trait MyConfigMapSuite extends MySuite with fixture.ConfigMapFixture
//
///**
// * StandardSuite with OpenCV automatically loaded.
// * This will fail if OpenCV isn't in the native path.
// */
//trait OpenCVStandardSuite extends StandardSuite {
//  loadOpenCV
//}
//
//trait TestUtil {
//  @deprecated("", "")
//  val random = new scala.util.Random(0)
//
//  @deprecated("", "")
//  def genNum[T: Numeric: Choose] = Gen.oneOf(Gen.negNum[T], Gen.posNum[T])
//
//  @deprecated("", "")
//  implicit def genDouble = genNum[Double]
//  @deprecated("", "")
//  implicit def genComplex = Gen(_ =>
//    Some(Complex(random.nextDouble, random.nextDouble)))
//  @deprecated("", "")
//  def genPowerOfTwo = Gen(_ => {
//    val power = (random.nextInt % 8).abs
//    val size = math.pow(2, power).toInt
//    assert(size > 0)
//    Some(size)
//  })
//  @deprecated("", "")
//  def genPowerOfTwoSeq[T: Gen] = {
//    def next = {
//      val size = genPowerOfTwo.sample.get
//
//      Gen.listOfN(size, implicitly[Gen[T]]).sample.map(_.toIndexedSeq)
//    }
//
//    Gen(_ => next)
//  }
//  @deprecated("", "")
//  def genPowerOfTwoSeqPair[T: Gen] = {
//    def next = {
//      val size = genPowerOfTwo.sample.get
//
//      def sample = Gen.listOfN(size, implicitly[Gen[T]]).sample.map(_.toIndexedSeq)
//      val Some(left) = sample
//      val Some(right) = sample
//      Some(left, right)
//    }
//
//    Gen(_ => next)
//  }
//  @deprecated("", "")
//  def genPowerOfTwoMatrix[T: Gen: ClassTag] = {
//    def next = {
//      val rows = genPowerOfTwo.sample.get
//      val cols = genPowerOfTwo.sample.get
//
//      for (data <- Gen.listOfN(rows * cols, implicitly[Gen[T]]).sample) yield {
//        assert(data.size == rows * cols)
//
//        data.toIndexedSeq.grouped(rows).toIndexedSeq.toMatrix
//      }
//    }
//
//    Gen(_ => next)
//  }
//  @deprecated("", "")
//  def genPowerOfTwoMatrixPair[T: Gen: ClassTag] = {
//    def next = {
//      val rows = genPowerOfTwo.sample.get
//      val cols = genPowerOfTwo.sample.get
//
//      def sample = for (data <- Gen.listOfN(rows * cols, implicitly[Gen[T]]).sample) yield {
//        assert(data.size == rows * cols)
//
//        data.toIndexedSeq.grouped(rows).toIndexedSeq.toMatrix
//      }
//
//      val Some(left) = sample
//      val Some(right) = sample
//      Some(left, right)
//    }
//
//    Gen(_ => next)
//  }
//}
//
//object TestUtil extends TestUtil