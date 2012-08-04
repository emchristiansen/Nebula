package nebula

object LSHMatch {
  type BinaryHistogram = Tuple2[Int, Int]
  
  def mkHistogram(threshold: Double, points: List[Double]): BinaryHistogram = {
    assume(threshold >= 0 && threshold <= 1)
    for (point <- points) { assume(point >= 0 && point <= 1) }
    val numLess = points.count(_ < threshold)
    (numLess, points.size - numLess)
  }
  
  def l1Distance(left: BinaryHistogram, right: BinaryHistogram): Int = {
    (left._1 - right._1).abs + (left._2 - right._2).abs
  }
  
  def mkHistogramVector(thresholds: List[Double])(points: List[Double]): List[BinaryHistogram] = {
    thresholds.map(threshold => mkHistogram(threshold, points))
  }
  
  def meanL1Distance(left: List[BinaryHistogram], right: List[BinaryHistogram]): Double = {
    assume(left.size == right.size)
    val sum = left.zip(right).map({case(l, r) => l1Distance(l, r)}).sum
    sum.toDouble / left.size
  }
}
