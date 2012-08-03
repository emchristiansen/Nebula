import java.awt.image.BufferedImage

case class WeightedEdge[A](val left: A, val right: A, val weight: Double)

object Match {
  def greedyMatch[A](mismatchPenalty: Double, edges: List[WeightedEdge[A]]): List[WeightedEdge[A]] = {
    val leftSeen = collection.mutable.Set[A]()
    val rightSeen = collection.mutable.Set[A]()
    val sortedEdges = edges.filter(_.weight < mismatchPenalty).sortBy(_.weight)

    val matches = 
      for (edge@WeightedEdge(left, right, _) <- sortedEdges
           if !leftSeen.contains(left) && !rightSeen.contains(right)) yield {
        leftSeen += left
	rightSeen += right
        edge
    }
    matches
  }

  def greedyMatchScore[A](mismatchPenalty: Double)(edges: List[WeightedEdge[A]]): Double = {
    val numLeft = edges.map(_.left).toSet.size
    val matching = greedyMatch(mismatchPenalty, edges)
    val matchCost = matching.map(_.weight).sum
    val mismatchCost = (numLeft - matching.size) * mismatchPenalty
//    println("num matched, mismatched: %s, %s".format(matching.size, numLeft - matching.size))
    matchCost + mismatchCost
  }

  def unconstrainedMatch[A](edges: List[WeightedEdge[A]]): List[WeightedEdge[A]] = {
    val leftSeen = collection.mutable.Set[A]()
    val sortedEdges = edges.sortBy(_.weight)

    val matches = 
      for (edge@WeightedEdge(left, _, _) <- sortedEdges
           if !leftSeen.contains(left)) yield {
        leftSeen += left
        edge
    }
    matches    
  }

  def unconstrainedMatchScore[A](edges: List[WeightedEdge[A]]): Double = {
    val matching = unconstrainedMatch(edges)
    matching.map(_.weight).sum / matching.size.toDouble
  }
}

case class Box(val ranges: IndexedSeq[Tuple2[Int, Int]]) {
  def atomic: Boolean = ranges.map(r => r._2 - r._1 <= 1).reduce(_ && _)
}
  
case class PyramidHistogram(val bins: Map[Int, Int]) {
//  def ++(that: PyramidHistogram): PyramidHistogram = {
//    val combined = new collection.mutable.HashMap[Box, Int] { override def default(key: Box): Int = 0 }
//    for ((key, value) <- bins.toList ++ that.bins.toList) { combined(key) += value }
//    PyramidHistogram(combined.toMap)
//  }
}

object PyramidHistogram {
  def createFromSinglePoint(weight: Int)(box: Box)(vector: IndexedSeq[Int]): PyramidHistogram = {
    assert(box.ranges.size == vector.size)    
    
    def createList(weight: Int, box: Box): List[Tuple2[Int, Int]] = {
      val thisScale = (box.hashCode, weight)
    
      val otherScales = if (box.atomic) {
        List[Tuple2[Int, Int]]()
      } else {
        val newRanges = for ((coordinate, (low, high)) <- vector.zip(box.ranges)) yield {
          val mid = (high + low) / 2
          if (coordinate < mid) (low, mid)
          else (mid, high)
        }
        createList(weight * 2, Box(newRanges))
      }
    
      thisScale :: otherScales      
    }
    
    PyramidHistogram(createList(weight, box).toMap)
  }
  
  def create(box: Box)(vectors: List[IndexedSeq[Int]]): PyramidHistogram = {
    val elements = vectors.par.map(v => createFromSinglePoint(1)(box)(v)).toList
    val bins = elements.flatMap(_.bins.toList).groupBy(bin => bin._1).mapValues(list => list.map(_._2).sum)
    PyramidHistogram(bins)
  }
  
  def distance(left: PyramidHistogram, right: PyramidHistogram): Double = {
    val leftSum = left.bins.values.sum.toDouble
    val rightSum = right.bins.values.sum.toDouble
    assert(leftSum == rightSum)
    val keys = left.bins.keys.toSet ++ right.bins.keys.toSet
    val errors = for (key <- keys.toList) yield {
      if (left.bins.contains(key) && right.bins.contains(key)) (left.bins(key) - right.bins(key)).abs
      else if (left.bins.contains(key)) left.bins(key)
      else right.bins(key)
//      if (left.bins.contains(key) && right.bins.contains(key)) (left.bins(key) / leftSum - right.bins(key) / rightSum).abs
//      else if (left.bins.contains(key)) left.bins(key) / leftSum
//      else right.bins(key) / rightSum
    }
    errors.sum
  }
}

case class EPHMatcher(val patchWidth: Int, val distanceWeight: Int) {
  import EpsilonL1Match._
  
  val epsilon = 1.0 / Util.nextPowerOfTwo(math.pow(patchWidth, 2) * 3 * 4)
  val scaleFactor = 2.0
  val scale = mkQuantizationScale(epsilon, scaleFactor)
  val permutationLength = patchWidth * patchWidth * 3
  val extractor = new SortExtractor(patchWidth)  
  
  def histogram(image: BufferedImage): Tuple2[Int, MultiScaleHistogram] = {
    val sort = extractor.extractDense(image)
    val height = sort.size
    val width = sort(0).size
    val spatialDescriptors = for (y <- 0 until height par; x <- 0 until width) yield {
      val sortPart = sort(y)(x).values.map(_.toDouble / permutationLength)
      val yPart = IndexedSeq.fill(distanceWeight)(y.toDouble / height)
      val xPart = IndexedSeq.fill(distanceWeight)(x.toDouble / width)
      sortPart ++ yPart ++ xPart
    }
    
    (spatialDescriptors.size, pointsToMultiScaleHistogram(scale, spatialDescriptors.map(_.toList).toList))
  }  
  
  def distance(left: BufferedImage, right: BufferedImage): Double = {
    val (leftDimension, leftHistogram) = histogram(left)
    val (rightDimension, rightHistogram) = histogram(right)
    assert(leftDimension == rightDimension)
    estimateL1Matching(scale, leftDimension, leftHistogram, rightHistogram)  
  }
}

case class PHMatcher(val patchWidth: Int, val distanceWeight: Int) {
  val permutationLength = patchWidth * patchWidth * 3
  val extractor = new SortExtractor(patchWidth)  
  
  def histogram(image: BufferedImage): PyramidHistogram = {
    val sort = extractor.extractDense(image)
    val height = sort.size
    val width = sort(0).size
    val spatialDescriptors = for (y <- 0 until height; x <- 0 until width) yield {
      val sortPart = sort(y)(x).values
      val spatialPart = IndexedSeq((distanceWeight * y.toDouble / height).toInt, (distanceWeight * x.toDouble / width).toInt)
      sortPart ++ spatialPart
    }
    
    def nextPowerOfTwo(n: Int): Int = {
      math.pow(2, (math.log(n) / math.log(2)).ceil).round.toInt
    }
    
    val radius = nextPowerOfTwo(List(permutationLength, distanceWeight).max)
    //println(radius)
    val box = Box(IndexedSeq.fill(permutationLength + 2)((0, radius)))
    PyramidHistogram.create(box)(spatialDescriptors.toList)
  }
  
  def distance(left: BufferedImage, right: BufferedImage): Double = {
    val leftHistogram = histogram(left)
	  val rightHistogram = histogram(right)
	  PyramidHistogram.distance(leftHistogram, rightHistogram)  
  }
}
