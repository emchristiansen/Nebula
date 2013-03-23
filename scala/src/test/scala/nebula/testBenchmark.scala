package nebula

import nebula._
import nebula.util._
import org.scalatest._
import org.scalatest.prop._
import javax.imageio.ImageIO
import java.io.File
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random
import breeze.linalg._

import org.scalacheck._
import breeze.math._

import org.apache.commons.math3.transform.DftNormalization
import org.apache.commons.math3.transform.FastFourierTransformer
import org.apache.commons.math3.transform.TransformType
import DenseMatrixUtil._
import reflect._

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
@WrapWith(classOf[ConfigMapWrapperSuite])
class TestBenchmark(
  override val configMap: Map[String, Any]) extends StandardSuite {
  
  test("Thread.sleep", MediumTest) {
    val sleepMs = 3
    
    val closure = () => Thread.sleep(sleepMs)
    
    val time = Benchmark.measure(closure, 100)
    
    assertRelativelyNear(1.1)(time, sleepMs)
  }
  
  test("robust across changing numIterations", MediumTest) {
    val closure = () => {
      val values = (0 until 10) map (_.toDouble)
      val mapped = values map (_ / 2.3)
      mapped.sum
    }
    
    val left = Benchmark.measure(closure, 1000)
    val right = Benchmark.measure(closure, 2000)
    
    assertRelativelyNear(1.1)(left, right)
  }
}