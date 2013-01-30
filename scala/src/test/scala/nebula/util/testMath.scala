package nebula.util

import nebula._
import nebula.util._
import org.scalatest.FunSuite
import javax.imageio.ImageIO
import java.io.File
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random
import breeze.linalg.DenseMatrix

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
class TestMath extends FunSuite {
  test("mean") {
    val in = Seq(1, 2, 3)
    assertNear(MathUtil.mean(in), 2)
  }
  
  test("l2Norm") {
    val in = Seq(1, 2, 3)
    assertNear(MathUtil.l2Norm(in.toArray), math.sqrt(1 + 4 + 9))
  }
}