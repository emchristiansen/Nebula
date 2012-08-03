import java.awt.image._

case class LM[A <: DescriptorTrait[_]](val extractor: DescriptorExtractor[A], 
									   val distance: (IndexedSeq[Tuple2[Int, Int]], IndexedSeq[A], IndexedSeq[A]) => IndexedSeq[Int], 
									   val radius: Int, 
									   matchScorer: (List[WeightedEdge[Tuple2[Int, Int]]]) => Double) {
  def weightedEdges(left: BufferedImage, right: BufferedImage): List[WeightedEdge[Tuple2[Int, Int]]] = {
    val leftDense = extractor.extractDense(left)
    val rightDense = extractor.extractDense(right)
    val leftHeight = leftDense.size
    val leftWidth = leftDense(0).size
    val rightHeight = rightDense.size
    val rightWidth = rightDense(0).size
    
    val locationPairs = (for (lY <- 0 until leftHeight par;
          lX <- 0 until leftWidth;
          rY <- (lY - radius) to (lY + radius);
          rX <- (lX - radius) to (lX + radius)
          if rY >= 0 && rY < rightHeight && rX >= 0 && rX < rightWidth) yield {
      val leftDescriptor = leftDense(lY)(lX)
      val rightDescriptor = rightDense(rY)(rX)
      ((lY, lX), (rY, rX))
    }).toIndexedSeq
    
    val indexPairs = for (((lY, lX), (rY, rX)) <- locationPairs) yield {
      (lY * leftWidth + lX, rY * rightWidth + rX)
    }
    val leftFlat = leftDense.flatten
    val rightFlat = rightDense.flatten 
    val distances = distance(indexPairs, leftFlat, rightFlat)
    
    locationPairs.zip(distances).map({case ((l, r), d) => WeightedEdge(l, r, d)}).toList
  }

  def distance(left: BufferedImage, right: BufferedImage): Double = {
    val edges = weightedEdges(left, right)
    val score = matchScorer(edges)
    score
  }
}

object LBM {
  def indexifyDistance[A](distance: (A, A) => Int): 
      (IndexedSeq[Tuple2[Int, Int]], IndexedSeq[A], IndexedSeq[A]) => IndexedSeq[Int] = {
    def indexified(indices: IndexedSeq[Tuple2[Int, Int]], left: IndexedSeq[A], right: IndexedSeq[A]): IndexedSeq[Int] = {
      indices.map({case (l, r) => distance(left(l), right(r))})
    }
    indexified _
  }
  
  def BLBM(numPairs: Int, patchWidth: Int, radius: Int, mismatchPenaltyFraction: Double): LM[Descriptor[Boolean]] = {
    val extractor = BRIEFExtractor(numPairs, patchWidth)
    val distance = indexifyDistance(DescriptorDistance.l0[Boolean] _)
    val maxError = numPairs
    val matchScorer = Match.greedyMatchScore[Tuple2[Int, Int]](mismatchPenaltyFraction * maxError) _
    LM(extractor, distance, radius, matchScorer)
  }

  def l0LBM(patchWidth: Int, radius: Int, mismatchPenaltyFraction: Double): LM[SortDescriptor] = {
    val extractor = new SortExtractor(patchWidth)
    val distance = indexifyDistance(DescriptorDistance.l0[Int] _)
    val maxError = 3 * math.pow(patchWidth, 2) // Approximate
    val matchScorer = Match.greedyMatchScore[Tuple2[Int, Int]](mismatchPenaltyFraction * maxError) _
    LM(extractor, distance, radius, matchScorer)
  }

  def l1LBM(patchWidth: Int, radius: Int, mismatchPenaltyFraction: Double): LM[SortDescriptor] = {
    val extractor = new SortExtractor(patchWidth)
    val distance = indexifyDistance(DescriptorDistance.l1 _)
    val maxError = math.pow(3 * math.pow(patchWidth, 2), 2) // Approximate
    val matchScorer = Match.greedyMatchScore[Tuple2[Int, Int]](mismatchPenaltyFraction * maxError) _
    LM(extractor, distance, radius, matchScorer)
  }

  def KTLBM(patchWidth: Int, radius: Int, mismatchPenaltyFraction: Double): LM[SortDescriptor] = {
    val extractor = new SortExtractor(patchWidth)
    val distance = indexifyDistance(DescriptorDistance.kendallTau _)
    val maxError = {
      val numElements = 3 * math.pow(patchWidth, 2)
      (numElements * (numElements - 1)) / 2
    }
    val matchScorer = Match.greedyMatchScore[Tuple2[Int, Int]](mismatchPenaltyFraction * maxError) _
    LM(extractor, distance, radius, matchScorer)
  }
}

object LUM {
  def l0LUM(patchWidth: Int, radius: Int): LM[SortDescriptor] = {
    val extractor = new SortExtractor(patchWidth)
    val distance = LBM.indexifyDistance(DescriptorDistance.l0[Int] _)
    val matchScorer = Match.unconstrainedMatchScore[Tuple2[Int, Int]] _
    LM(extractor, distance, radius, matchScorer)
  }
  
  def l0LUMOCL(patchWidth: Int, radius: Int): LM[SortDescriptor] = {
    val extractor = new SortExtractorOCL(patchWidth)
//    val extractor = new SortExtractor(patchWidth)
//    val distance = LBM.indexifyDistance(DescriptorDistance.l0[Int] _)    
    val distance = DescriptorDistance.l0OCL _
    val matchScorer = Match.unconstrainedMatchScore[Tuple2[Int, Int]] _
    LM(extractor, distance, radius, matchScorer)
  }
  
  def l1LUM(patchWidth: Int, radius: Int): LM[SortDescriptor] = {
    val extractor = new SortExtractor(patchWidth)
    val distance = LBM.indexifyDistance(DescriptorDistance.l1 _)
    val matchScorer = Match.unconstrainedMatchScore[Tuple2[Int, Int]] _
    LM(extractor, distance, radius, matchScorer)
  }
}
