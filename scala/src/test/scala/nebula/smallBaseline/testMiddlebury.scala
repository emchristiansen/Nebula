package nebula.smallBaseline

import org.apache.commons.math3.linear.{ Array2DRowRealMatrix, ArrayRealVector }
import org.scalatest.FunSuite
import nebula._
import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck._
import math._
import java.io.File
import nebula.smallBaseline.FlowField
import nebula.smallBaseline.SmallBaselinePair

///////////////////////////////////////////////////////////

class TestMiddlebury extends FunSuite {
  ignore("construct FlowField from file") {
    val filename = "/middleburyImages/other-gt-flow/Dimetrodon/flow10.flo.txt"
    val file = new File(getClass.getResource(filename).getFile)
    FlowField(file)
  }

  ignore("construct SmallBaselinePair from file") {
    val rootDirectory = new File(getClass.getResource("/middleburyImages").getFile)
    SmallBaselinePair(rootDirectory, "Dimetrodon")
  }

  ignore("the distance from a FlowField to itself should be zero") {
    val flow = {
      val filename = "/middleburyImages/other-gt-flow/Dimetrodon/flow10.flo.txt"
      val file = new File(getClass.getResource(filename).getFile)
      FlowField(file)
    }
    
    assert(flow.mse(flow).abs === 0)
  }
}