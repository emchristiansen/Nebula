package nebula

import nebula._

import org.scalatest.FunSuite
 
///////////////////////////////////////////////////////////

class TestEpsilonL1Match extends FunSuite {
   
//   test("calculateL1") {
//     val left = mkHistogram(List(1, 2, 1, 3, 3))
//     val right = mkHistogram(List(2, 2, 1, 3, 1))
//     val third = mkHistogram(List(1, 1, 1, 1, 1))
//     assert(l1HistogramDistance(left, left) === 0)
//     assert(l1HistogramDistance(left, right) === 2)
//     assert(l1HistogramDistance(left, third) === 3 + 1 + 2)
//   }
  
  // test("estimateL1") {
  //   val dimension = 10
  //   val numPerSet = 50
  //   def mkVector = for (_ <- 0 until dimension toList) yield Global.random.nextDouble
    
  //   val leftSet = for (_ <- 0 until numPerSet toList) yield mkVector
  //   val rightSet = for (_ <- 0 until numPerSet toList) yield mkVector
    
  //   val cost = for (l <- leftSet toArray) yield {
  //     for (r <- rightSet toArray) yield {
  //       l1Distance(l, r)
  //     }
  //   }
    
  //   val optimalAssignment = HungarianAlgorithm.hgAlgorithm(cost, "min").map(_.toList).toList
    
  //   val epsilon = 1.0 / 8.0
  //   val scaleFactor = 2.0
  //   val scale = mkQuantizationScale(epsilon, scaleFactor)
  //   val leftHist = pointsToMultiScaleHistogram(scale, leftSet)
  //   val rightHist = pointsToMultiScaleHistogram(scale, rightSet)

  //   val optimalCost = (for (List(i, j) <- optimalAssignment) yield { cost(i)(j) }).sum
  //   val estimatedCost = estimateL1Matching(scale, dimension, leftHist, rightHist)
    
  //   assert(optimalCost < 2 * estimatedCost && estimatedCost < 2 * optimalCost)
  // }
}
