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
  
  test("typeName from instance", FastTest) {    
    val x = (List(1, 2), 1.2, ("hi", List(2.2)))
    
    val y = null.asInstanceOf[List[Double]]
    println(StaticTypeName.typeNameFromConcreteInstance(x))
    
//    println(StaticTypeName.typeStringNoInstance[List[Float]])
    
//    StaticTypeName.printType[List[Char]]
    
    println(StaticTypeName.typeNameFromConcrete[List[List[Int]]])
    
    import StaticTypeName._
    
    println(typeNameFromConcrete[List[String]])
    
//    println(typeStringFromConcreteInstanceTODO(List[Int]()))
//    
//    val foo = implicitly[List[Int] => TypeString]
    
//    implicitly[Int => TypeName[_]]
    
//    val foo = implicitly[TypeName[List[Char]]]
//    println(foo)
    
//    implicitly[Int]
    
//    implicitly[Foo[Int]]
    
//    implicitly[List[String]]
    
//    println(typeName2[List[Float]])
    
//    def printType[A] = println(StaticTypeName.typeNameNoInstance[A]) 
//    
//    printType[List[Float]]
//    
//    assert(StaticTypeName.typeNameFromInstance(x) == 
//      "(List[Int], Double, (String, List[Double]))")
  }
  
//  test("typeName without instance", FastTest) {
////    println(typeName2[List[Int]])
//    println(StaticTypeName.typeName(null))
//    
////    assert(typeName2[List[Int]].contains("List[Int]"))
//  }
}