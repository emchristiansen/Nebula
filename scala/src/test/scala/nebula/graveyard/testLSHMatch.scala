package nebula.graveyard

import nebula._

import org.scalatest.FunSuite
import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner 

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
class TestLSHMatch extends FunSuite {
  // import LSHMatch._
  
  // val leftPoints = List(0.1, 0.8, 0.1)
  // val rightPoints = List(0.2, 0.8, 0.3)
  
  // test("meanL1Distance") {
  //   val cost = Array(Array(1.0, 4.0, 5.0), Array(5.0, 7.0, 6.0), Array(5.0, 8.0, 8.0))
  //   val out = HungarianAlgorithm.hgAlgorithm(cost, "min")
    
  //   val thresholds = for (_ <- 0 until 10000 toList) yield Global.random.nextDouble
  //   val mkHist = mkHistogramVector(thresholds) _
  //   val leftHist = mkHist(leftPoints)
  //   val rightHist = mkHist(rightPoints)
  // }
}
