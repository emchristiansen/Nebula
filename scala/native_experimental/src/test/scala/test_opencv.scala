import org.apache.commons.math3.linear.{ Array2DRowRealMatrix, ArrayRealVector }
import org.scalatest.FunSuite

import nebula.{ Homography, KeyPointUtil }

class TestOpenCV extends FunSuite {
  test("scaleFactor") {
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
      assert(KeyPointUtil.scaleFactor(homography, xyPoint) === 1)
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
      assert(KeyPointUtil.scaleFactor(homography, xyPoint) === 3.5)
    }
  }
}
