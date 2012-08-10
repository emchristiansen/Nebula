import nebula._

import org.scalatest.FunSuite
 
import javax.imageio.ImageIO
import java.io.File

import com.googlecode.javacv.cpp.opencv_features2d._

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}

case class Person(firstName: String, lastName: String, int: Int, double: Double)

class TestAnything extends FunSuite { 
  test("anything") {
    val p = Person("Eric", "Bob", 42, 3.14)

    val dmatch = new DMatch(1, 2, 3.14f)

    implicit val formats = Serialization.formats(ShortTypeHints(List(classOf[DMatch]))) + new DMatchSerializer
    val json = write(dmatch)
    println(json)

    val loaded = read[DMatch](json)
    println(loaded.distance)
  }
}
