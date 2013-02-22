package nebula.util

import nebula._

import org.scalacheck._
import org.scalatest.prop._
import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import breeze.linalg._

////////////////////////////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
@WrapWith(classOf[ConfigMapWrapperSuite])
class TestDenseMatrixUtil(
  override val configMap: Map[String, Any]) extends OpenCVStandardSuite {
  test("matToMatrixComplex on simple example", FastTest, InteractiveTest) {
    val data = Seq(-100, 630, -100, 30, -120, -20, 50, -160).map(_.toDouble)
    val matrix = new DenseMatrix(2, data.toArray) mapValues (_.toDouble)
    val mat = DenseMatrixUtil.matrixDoubleToMat(matrix)
    val complexMatrix = DenseMatrixUtil.matToMatrixComplex(mat)
    
//    val golden = data.grouped(2).toSeq.transpose.flatten.grouped(2) map {
//      case Seq(real, imaginary) => new SpireComplex(real, imaginary)
//    } 
    
    println(matrix)
    printBlock(complexMatrix)
  }
}