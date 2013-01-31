package nebula.util

import org.apache.commons.math3.linear.{ Array2DRowRealMatrix, ArrayRealVector }
import org.scalatest._
import nebula._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
class TestOpenCV extends FunSuite {
  test("scaleFactor", InstantTest) {
    val xyPoint = new ArrayRealVector(Array(3.0, 4.0))

    {
      val homography = {
        val matrix = new Array2DRowRealMatrix(
          Array(
            Array(1.0, 0.0, 0.0),
            Array(0.0, 1.0, 0.0),
            Array(0.0, 0.0, 1.0)),
          true)
        Homography(matrix)
      }
//      val (location, size, angle) = locationAndSizeAndAngleUnderHomography(
//          homography,
//          xyPoint,
//          1,
//          0)
//      assert(size === 1)
    }

    {
      val homography = {
        val matrix = new Array2DRowRealMatrix(
          Array(
            Array(0.5, 0.0, 10),
            Array(0.0, 7.0, -20),
            Array(0.0, 0.0, 1.0)),
          true)
        Homography(matrix)
      }
//      val (location, size, angle) = locationAndSizeAndAngleUnderHomography(
//          homography,
//          xyPoint,
//          1,
//          0)
//      assert(size === 3.5)
    }
  }
}
