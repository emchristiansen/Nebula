import nebula._

import org.scalatest.FunSuite
 
import javax.imageio.ImageIO
import java.io.File

import com.codahale.jerkson.Json._

case class Person(firstName: String, lastName: String, int: Int, double: Double)

class TestAnything extends FunSuite { 
  test("anything") {
    val p = Person("Eric", "Bob", 42, 3.14)

    val j = generate(p)

    println(j)

    val p2 = parse[Person](j)

    println(p2)
  }
}
