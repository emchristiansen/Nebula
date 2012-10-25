import org.apache.commons.math3.linear.{ Array2DRowRealMatrix, ArrayRealVector }
import org.scalatest.FunSuite
import nebula.KeyPointUtil._
import nebula.{ Homography, KeyPointUtil }
import nebula.MatcherParameterized._
import nebula._
import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck._
import math._
import java.io.File
import nebula.wideBaseline.SmallBaselinePair
import nebula.wideBaseline.FlowField

class TestMiddlebury extends FunSuite {
  test("construct FlowField from file") {
    val filename = "/middleburyImages/other-gt-flow/Dimetrodon/flow10.flo.txt"
    val file = new File(getClass.getResource(filename).getFile)
    FlowField.fromFloFile(file)
  }

  test("construct SmallBaselinePair from file") {
    val rootDirectory = new File(getClass.getResource("/middleburyImages").getFile)
    SmallBaselinePair.fromName(rootDirectory, "Dimetrodon")
  }

  test("the distance from a FlowField to itself should be zero") {
    val flow = {
      val filename = "/middleburyImages/other-gt-flow/Dimetrodon/flow10.flo.txt"
      val file = new File(getClass.getResource(filename).getFile)
      FlowField.fromFloFile(file)
    }
    
    assert(flow.l2Distance(flow).abs === 0)
  }
}