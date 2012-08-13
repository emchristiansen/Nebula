import nebula._

import org.scalatest.FunSuite
 
import javax.imageio.ImageIO
import java.io.File

import com.googlecode.javacv.cpp.opencv_features2d._

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}

class TestAnything extends FunSuite { 
  test("anything") {
    val d = IndexedSeq(1, 2, 3)
    CompileTest.foo(d)

    val s = MySortDescriptor(IndexedSeq(0, 2, 1))
    CompileTest.foo(s)
  }
}
