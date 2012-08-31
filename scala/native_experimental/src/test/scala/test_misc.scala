import nebula._

import org.scalatest.FunSuite
 
import javax.imageio.ImageIO
import java.io.File

import com.googlecode.javacv.cpp.opencv_features2d._

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}

case class Person(firstName: String, lastName: String, int: Int, double: Double)

class TestMisc extends FunSuite { 
  test("caseClassToStringMap") {
    val person = Person("Eric", "Christiansen", 42, 3.14)

    val map = Util.caseClassToStringMap(person)
    val goldenMap = Map("jsonClass" -> "Person", "firstName" -> "Eric", "lastName" -> "Christiansen", "int" -> "42", "double" -> "3.14")

    assert(map === goldenMap)

    val experiment1 = CorrespondenceExperiment(
      "bikes", 
      2, 
      FASTDetector(100),
      SortExtractor(false, false, 8, 5, "Gray"),
      L0Matcher())

    val experiment2 = experiment1.copy(otherImage = 4)
    // val title = SummaryUtil.tableTitle(Seq(experiment1, experiment2))
    // val golden = "D-FASTDetector-MKP-100_E-SortExtractor-BW-5-C-true-NR-false-NS-false-PW-8_IC-bikes_M-L0Matcher_OI-*"
    // assert(title === golden)
  }

  test("numToBits") {
    assert(Util.numToBits(3)(15) === Seq(true, true, true))
    assert(Util.numToBits(5)(15) === Seq(false, true, true, true, true))
    assert(Util.numToBits(0)(12) === Seq())
    assert(Util.numToBits(5)(10) === Seq(false, true, false, true, false))
  }
}
