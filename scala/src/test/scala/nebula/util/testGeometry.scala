package nebula.util

import nebula._
import org.scalatest.FunSuite
import javax.imageio.ImageIO
import java.io.File

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import nebula.util._
import nebula.imageProcessing._
import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.apache.commons.math3.linear.ArrayRealVector
import org.apache.commons.math3.linear.LUDecomposition
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.linear.RealMatrix
import org.apache.commons.math3.linear.RealVector
import org.apache.commons.math3.linear.SingularValueDecomposition
import org.opencv.features2d.KeyPoint
import java.awt.geom.AffineTransform
import org.scalacheck._
import org.scalatest.prop._
import org.scalatest._
import nebula.testing._
import scala.util.Random

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
class TestGeometry extends FunSuite {
  val image: Image = ImageIO.read(getResource("/iSpy.png"))

//  val genKeyPoint = Generators.genKeyPoint(image.getWidth, image.getHeight, 100)

  test("fit lots of homographies", FastTest) {
    def randomKeyPoint = new KeyPoint(
      1000 * new Random().nextFloat,
      1000 * new Random().nextFloat,
      0,
      0)

    def random4 = 4 times randomKeyPoint

    for (_ <- 0 until 20) {
      val leftKeyPoints = random4 
      val rightKeyPoints = random4

      val homography = Geometry.fitHomographyFromFourTranslations(
        leftKeyPoints,
        rightKeyPoints)

//      println(homography)
    }

    //    implicit val generatorDrivenConfig =
    //      PropertyCheckConfig(minSuccessful = 20, maxSize = 5)
    //    forAll(Gen.listOfN(8, genKeyPoint)) { keyPoints =>
    //      whenever(keyPoints.size == 8) {
    //
    //      }
    //    }
  }

  test("homography from 4 points", FastTest) {
    val golden = {
      val matrix = new Array2DRowRealMatrix(Array[Double](
        2.0, 0.1, -5,
        -0.4, 0.6, 10,
        0, 0, 1).grouped(3).toArray)
      Homography(matrix)
    }

    def keyPointFromXY(x: Double, y: Double): KeyPoint = new KeyPoint(
      x.toFloat,
      y.toFloat,
      0,
      0,
      0,
      0)

    val leftKeyPoints = Seq(
      keyPointFromXY(0, 0),
      keyPointFromXY(0, 32),
      keyPointFromXY(10, 0),
      keyPointFromXY(11, 11))

    val rightKeyPoints = leftKeyPoints map (golden.transformXYOnly)

    val homography = Geometry.fitHomographyFromFourTranslations(
      leftKeyPoints,
      rightKeyPoints)

    println(golden)
    println(homography)
  }

  test("homography from 2 points", FastTest) {
    val golden = {
      val matrix = new Array2DRowRealMatrix(Array[Double](
        2.0, 0.1, -5,
        -0.4, 0.6, 10,
        0.2, -0.3, 1).grouped(3).toArray)
      Homography(matrix)
    }

    println(golden)

    val leftA = new KeyPoint(
      1,
      0,
      1,
      0)

    val rightA = golden.transform(leftA)

    val leftB = new KeyPoint(
      1,
      1,
      1,
      0)

    val rightB = golden.transform(leftB)

    println(leftA)
    println(leftB)
    println(rightA)
    println(rightB)

    val homography = Geometry.fitHomographyFromTwoSimilarityCorrespondences(
      Seq(leftA, leftB),
      Seq(rightA, rightB))

    println(homography)
  }

  //  ignore("homography from 2 points", FastTest) {
  //    val transform = {
  //      val base = new AffineTransform
  //      base.rotate(math.Pi / 2)
  //      base.scale(3, 3)
  //      base
  //    }
  //
  //    val golden = {
  //      val flatMatrix = Array[Double](0, 0, 0, 0, 0, 0)
  //      transform.getMatrix(flatMatrix)
  //      val columnMajor = Array(
  //        flatMatrix(0), flatMatrix(1), 0,
  //        flatMatrix(2), flatMatrix(3), 0,
  //        flatMatrix(4), flatMatrix(5), 1)
  //      val data = new Array2DRowRealMatrix(
  //        columnMajor.grouped(3).toArray.transpose)
  //      Homography(data)
  //    }
  //
  //    println(golden)
  //
  //    val leftA = new KeyPoint(
  //      1,
  //      0,
  //      1,
  //      0)
  //
  //    val rightA = new KeyPoint(
  //      0,
  //      3,
  //      3,
  //      (math.Pi / 2).toFloat)
  //
  //    val leftB = new KeyPoint(
  //      2,
  //      0,
  //      1,
  //      0)
  //
  //    val rightB = new KeyPoint(
  //      0,
  //      6,
  //      3,
  //      (math.Pi / 2).toFloat)
  //
  //    val homography = Geometry.fitHomographyFromTwoSimilarityCorrespondences(
  //      Seq(leftA, leftB),
  //      Seq(rightA, rightB))
  //      
  //    println(homography)
  //  }
}