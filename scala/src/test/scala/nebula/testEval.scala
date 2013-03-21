package nebula

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
class TestEval(
  override val configMap: Map[String, Any]) extends StandardSuite {
  import TestUtil._
  
  test("Imports", FastTest) {
    val importString = """
import foo
import bar;
      ;;
bur
"""
    val imports = Imports(importString)
    
    asserty(imports == Imports(Set("foo", "bar", "bur")))
  }
}