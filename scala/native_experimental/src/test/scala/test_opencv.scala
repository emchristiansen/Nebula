import nebula._

import org.scalatest.FunSuite
 
import javax.imageio.ImageIO
import java.io.File

import com.googlecode.javacv.cpp.opencv_features2d._

import org.apache.commons.math3.linear._

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}

class TestOpenCV extends FunSuite {
  test("cvMatToSeq") {
    val matString = "[ 235.0, 127.0, 32.0 ]"
    val seq = OpenCVUtil.cvMatStringToSeq(matString)
    val golden = Seq(235.0, 127.0, 32.0)
    assert(seq === golden)
  }

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
