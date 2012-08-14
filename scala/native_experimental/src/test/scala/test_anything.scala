import nebula._

import org.scalatest.FunSuite
 
import javax.imageio.ImageIO
import java.io.File

import com.googlecode.javacv.cpp.opencv_features2d._

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}

import com.twitter.util._

class TestAnything extends FunSuite { 
  val source = """
  import nebula._

    val experiment1 = CorrespondenceExperiment[FASTDetector, SortDescriptor, SortExtractor, L0Matcher](
      "bikes", 
      2, 
      FASTDetector(100),
      SortExtractor(false, false, 8, 5, true),
      L0Matcher())
  experiment1
"""

  test("anything") {
    val x = (new Eval).apply[CorrespondenceExperiment[FASTDetector, SortDescriptor, SortExtractor, L0Matcher]](source)
    println(x)
  }
}
