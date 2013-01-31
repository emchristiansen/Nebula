package nebula

import nebula._
import org.scalatest.FunSuite
import javax.imageio.ImageIO
import java.io.File

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import shapeless._

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
class TestAnything extends FunSuite {
  val image = ImageIO.read(new File(
    getClass.getResource("/iSpy.png").getFile))  
    
  test("blah", FastTest) {
    val m = Matcher.L1
    m.to[Matcher[IndexedSeq[Int]]]
  }
}


