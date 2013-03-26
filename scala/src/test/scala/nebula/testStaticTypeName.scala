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
class TestStaticTypeName(
  override val configMap: Map[String, Any]) extends StandardSuite {
  
  test("typeName", FastTest) {
    val x = (List(1, 2), 1.2, ("hi", List(2.2)))
    
    asserty(StaticTypeName.typeName(x) == 
      "(List[Int], Double, (String, List[Double]))")
  }
}