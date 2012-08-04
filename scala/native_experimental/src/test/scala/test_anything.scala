import nebula._

import org.scalatest.FunSuite
 
import javax.imageio.ImageIO
import java.io.File

import com.codahale.jerkson.Json._

case class Person(firstName: String, lastName: String)

class TestAnything extends FunSuite { 
  test("anything") {
    val p = Person("Eric", "Bob")

    val j = generate(p)

    println(j)

    val p2 = parse[Person](j)

    println(p2)
  }
}
