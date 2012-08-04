package nebula

object EpsilonL1Match {
  type Point = List[Double]
  type Bin = List[Int]
  type HashedBin = Int
  type Scale = List[Double]
  type Histogram[A] = Map[A, Int]
  type MultiScaleHistogram = List[Histogram[HashedBin]]
  type MultiScaleBin = List[HashedBin]
  
  def quantizeCoordinate(epsilon: Double, coordinate: Double): Int = {
    assume(epsilon > 0 && epsilon <= 1)
    assume(coordinate >= 0 && coordinate < 1)
    (coordinate / epsilon).floor.toInt
  }
  
  def quantize(epsilon: Double, point: Point): Bin = {
    point.map(coordinate => quantizeCoordinate(epsilon, coordinate))
  }
  
  def quantizeAndHash(epsilon: Double, point: Point): HashedBin = {
    quantize(epsilon, point).hashCode 
  }
  
  def mkQuantizationScale(epsilon: Double, scaleFactor: Double): Scale = {
    assume(epsilon > 0 && epsilon <= 1)
    def powers(start: Double): Stream[Double] = start #:: powers(start * scaleFactor)
    powers(epsilon).takeWhile(_ <= 1).toList
  }
  
  def quantizeMultiScale(scale: Scale, point: Point): MultiScaleBin = {
    scale.map(epsilon => quantizeAndHash(epsilon, point))
  }
  
  def mkHistogram[A](bins: List[A]): Histogram[A] = {
    bins.groupBy(x => x).mapValues(_.size)
  }
  
  def mkMultiScaleHistogram(multiScaleBins: List[MultiScaleBin]): MultiScaleHistogram = {
    multiScaleBins.transpose.map(mkHistogram)
  }
  
  def pointsToMultiScaleHistogram(
      scale: Scale, points: List[Point]): MultiScaleHistogram = {
    mkMultiScaleHistogram(points.par.map(point => quantizeMultiScale(scale, point)).toList)
  }
  
  def histogramIntersection[A](left: Histogram[A], right: Histogram[A]): Int = {
    val keys = (left.keys.toSet ++ right.keys.toSet).toList
    val intersections = for (key <- keys) yield {
      if (left.contains(key) && right.contains(key)) List(left(key), right(key)).min
      else 0
    }
    intersections.sum
  }
  
  def l1Distance(left: Point, right: Point): Double = {
    left.zip(right).map({case (l, r) => (l - r).abs}).sum
  }
  
  def estimateL1Matching(scale: Scale, 
                         dimension: Int, 
                         left: MultiScaleHistogram, 
                         right: MultiScaleHistogram): Double = {
    val intersections = left.zip(right).map({case (l, r) => histogramIntersection(l, r)})
//    println(scale)
//    println(intersections)
    val newIntersections = (0 :: intersections).sliding(2).map({case List(l, r) => r - l}).toList
    assert(newIntersections.size == scale.size)
    val distanceConstant = 1.0 / 3.0 // Expected distance between two points in unit interval.
    scale.zip(newIntersections).map({case (s, i) => s * i * dimension * distanceConstant}).sum
  }
}
