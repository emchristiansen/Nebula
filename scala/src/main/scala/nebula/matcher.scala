package nebula

import org.opencv.features2d.DMatch

import collection._
import PermutationLike.sortDescriptor
import graveyard._
import mpie._
import summary._
import smallBaseline._
import util._
import util.imageProcessing._
import wideBaseline._

///////////////////////////////////////////////////////////

sealed trait Matcher {
  def doMatch: Matcher.MatcherAction
}

object Matcher {
  type MatcherAction = (Boolean, Seq[Descriptor], Seq[Descriptor]) => Seq[DMatch]
  
  def applyIndividual(
    distanceMethod: (Descriptor, Descriptor) => Double,
    allPairs: Boolean,
    leftDescriptors: Seq[Descriptor],
    rightDescriptors: Seq[Descriptor]): Seq[DMatch] = {
    if (allPairs) {
      for (
        (left, leftIndex) <- leftDescriptors.zipWithIndex;
        (right, rightIndex) <- rightDescriptors.zipWithIndex
      ) yield {
        val distance = distanceMethod(left, right)
        new DMatch(leftIndex, rightIndex, distance.toFloat)
      }
    } else {
      for (((left, right), index) <- leftDescriptors.zip(rightDescriptors).zipWithIndex) yield {
        val distance = distanceMethod(left, right)
        new DMatch(index, index, distance.toFloat)
      }
    }
  }  
  
  val instances: List[java.lang.Class[_]] = nebula.TODO 
    
//    List(
//    classOf[L0Matcher],
//    classOf[L1Matcher],
//    classOf[L1IntervalMatcher],
//    classOf[L2Matcher],
//    classOf[KendallTauMatcher],
//    classOf[CayleyMatcher],
//    classOf[CayleyRotate4Matcher],
//    classOf[RobustCayleyMatcher],
//    classOf[GeneralizedL0Matcher])

  def l0(left: IndexedSeq[Any], right: IndexedSeq[Any]): Int =
    (left, right).zipped.count({ case (l, r) => l != r })

  def l1[A <% Double](left: IndexedSeq[A], right: IndexedSeq[A]): Double =
    (left, right).zipped.map({ case (l, r) => (l - r).abs }).sum

  def l2[A <% Double](left: IndexedSeq[A], right: IndexedSeq[A]): Double = {
    math.sqrt((left, right).zipped.map({ case (l, r) => math.pow(l - r, 2) }).sum)
  }

  def kendallTau(left: SortDescriptor, right: SortDescriptor): Int = {
    val size = left.size
    assert(size == right.size)
    val errors = for (i <- 0 until size; j <- i + 1 until size) yield {
      if (left(i) < left(j) == right(i) < right(j)) 0
      else 1
    }
    errors.sum
  }

  def cayley(left: SortDescriptor, right: SortDescriptor): Int = {
    assert(left.size == right.size)

    val rightInverse = right.invert
    val composition = left.compose(rightInverse)
    left.size - composition.numCycles
  }

  // Rotate values taken from a square patch pi / 2 radians.
  // TODO: This "rotate4" idea can be generalized to rotateN on general patches.
  def rotateQuarter(descriptor: SortDescriptor): SortDescriptor = {
    // TODO: Doing a color check in this part of the code is clearly dumb.
    val (patchWidth, pixels) = {
      val sqrt = math.sqrt(descriptor.size)
      val validGray = sqrt == sqrt.toInt

      val sqrtThird = math.sqrt(descriptor.size / 3)
      val validColor = sqrtThird == sqrtThird.toInt

      require(validGray || validColor)
      if (sqrt == sqrt.toInt) (sqrt.toInt, descriptor.grouped(1).toIndexedSeq)
      else (sqrtThird.toInt, descriptor.grouped(3).toIndexedSeq)
    }

    val indices = (0 until pixels.size).grouped(patchWidth).toSeq
    val rotated = indices.transpose.map(_.reverse)
    SortDescriptor(rotated.flatten.map(i => pixels(i)).flatten.toIndexedSeq)
  }

  def rotate4(descriptor: SortDescriptor): Seq[SortDescriptor] = {
    lazy val rotaters: Stream[SortDescriptor => SortDescriptor] =
      Stream.cons(identity, rotaters.map(f => rotateQuarter _ compose f))
    rotaters.map(_(descriptor)).take(4).toList
  }

  def cayleyRotate4(left: SortDescriptor, right: SortDescriptor): Int = {
    val rightRotations = rotate4(right)
    val distances = rightRotations.map(r => cayley(left, r))
    distances.min
  }

  def robustCayley(left: IndexedSeq[Int], right: IndexedSeq[Int]): Int = {
    val distances = for (
      leftSort <- Util.allSorts(left);
      rightSort <- Util.allSorts(right)
    ) yield {
      cayley(SortDescriptor(leftSort.toIndexedSeq), SortDescriptor(rightSort.toIndexedSeq))
    }
    distances.min
  }

  def generalizedL0(left: IndexedSeq[Int], right: IndexedSeq[Int]): Int = {
    val (leftSorted, rightPermuted) = left.zip(right).sortBy(_._1).unzip
    val groupSizes = Util.group(leftSorted.toList).map(_.size)

    val rightPermutedGroups = Util.groupBySizes(groupSizes, rightPermuted)
    val rightSortedGroups = Util.groupBySizes(groupSizes, rightPermuted.sorted)

    val permutedHistograms = rightPermutedGroups.map(EpsilonL1Match.mkHistogram)
    val sortedHistograms = rightSortedGroups.map(EpsilonL1Match.mkHistogram)

    permutedHistograms.zip(sortedHistograms).map({ case (l, r) => EpsilonL1Match.l1HistogramDistance(l, r) }).sum / 2
  }

  def generalizedCayley(left: IndexedSeq[Int], right: IndexedSeq[Int]): Int = {
    val rightPermuted = left.zip(right).sortBy(_._1).map(_._2)
    Util.numTranspositionsToSort(rightPermuted)
  }

  ///////////////////////////////////////////////////////////  

  case class IntervalRanking(values: IndexedSeq[Tuple2[Int, Int]]) {
    values.foreach(t => assert(t._1 <= t._2))
  }

  def intervalRanking(descriptor: RawDescriptor[Int]): IntervalRanking = {
    val distinctPixelValues = descriptor.values.toSet.toList
    val rank = SortDescriptor.fromUnsorted(SortDescriptor.fromUnsorted(descriptor.values))
    val intervals = Array.fill(rank.size)((-1, -1))
    for (value <- distinctPixelValues) {
      val indices = descriptor.values.zipWithIndex.filter(_._1 == value).map(_._2)
      val minRank = indices.map(rank.apply).min
      val maxRank = indices.map(rank.apply).max
      indices.foreach(i => intervals(i) = (minRank, maxRank))
    }
    IntervalRanking(intervals.toIndexedSeq)
  }

  def l1IntervalDistance(left: IndexedSeq[Int], right: IndexedSeq[Int]): Int = {
    val leftIntervals = intervalRanking(RawDescriptor(left))
    val rightIntervals = intervalRanking(RawDescriptor(right))

    def l1Distance(leftInterval: Tuple2[Int, Int], rightInterval: Tuple2[Int, Int]): Int = {
      // Dumb but def correct.
      (for (
        l <- leftInterval._1 to leftInterval._2;
        r <- rightInterval._1 to rightInterval._2
      ) yield math.abs(l - r)).min
    }

    leftIntervals.values.zip(rightIntervals.values).map({ case (l, r) => l1Distance(l, r) }).sum
  }
}

/////////////////////////////////////////////////////////////////////////////

// TODO: Lots of duplication.
case class L0Matcher() extends Matcher {
  import Matcher._
  
  override def doMatch = (
    allPairs: Boolean,
    leftDescriptors: Seq[Descriptor],
    rightDescriptors: Seq[Descriptor]) => {
    applyIndividual(
      (x: Descriptor, y: Descriptor) =>
        Matcher.l0(
          x.values[Any],
          y.values[Any]),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class L1Matcher() extends Matcher {
  import Matcher._
  
  def doMatch = (
    allPairs: Boolean,
    leftDescriptors: Seq[Descriptor],
    rightDescriptors: Seq[Descriptor]) => {
    applyIndividual(
      (x: Descriptor, y: Descriptor) =>
        Matcher.l1(
          x.values[Double],
          y.values[Double]),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class L1IntervalMatcher() extends Matcher {
  import Matcher._
  
  def doMatch = (
    allPairs: Boolean,
    leftDescriptors: Seq[Descriptor],
    rightDescriptors: Seq[Descriptor]) => {
    applyIndividual(
      (x: Descriptor, y: Descriptor) =>
        Matcher.l1IntervalDistance(
          x.values[Int],
          y.values[Int]),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class L2Matcher() extends Matcher {
  import Matcher._
  
  def doMatch = (
    allPairs: Boolean,
    leftDescriptors: Seq[Descriptor],
    rightDescriptors: Seq[Descriptor]) => {
    applyIndividual(
      (x: Descriptor, y: Descriptor) =>
        Matcher.l2(
          x.values[Double],
          y.values[Double]),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class KendallTauMatcher() extends Matcher {
  import Matcher._
  
  def doMatch = (
    allPairs: Boolean,
    leftDescriptors: Seq[Descriptor],
    rightDescriptors: Seq[Descriptor]) => {
    applyIndividual(
      (x: Descriptor, y: Descriptor) => Matcher.kendallTau(
        x.asType[SortDescriptor], y.asType[SortDescriptor]),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class CayleyMatcher() extends Matcher {
  import Matcher._
  
  def doMatch = (
    allPairs: Boolean,
    leftDescriptors: Seq[Descriptor],
    rightDescriptors: Seq[Descriptor]) => {
    applyIndividual(
      (x: Descriptor, y: Descriptor) => Matcher.cayley(
        x.asType[SortDescriptor], y.asType[SortDescriptor]),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class CayleyRotate4Matcher() extends Matcher {
  import Matcher._
  
  def doMatch = (
    allPairs: Boolean,
    leftDescriptors: Seq[Descriptor],
    rightDescriptors: Seq[Descriptor]) => {
    applyIndividual(
      (x: Descriptor, y: Descriptor) => Matcher.cayleyRotate4(
        x.asType[SortDescriptor], y.asType[SortDescriptor]),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class RobustCayleyMatcher() extends Matcher {
  import Matcher._
  
  def doMatch = (
    allPairs: Boolean,
    leftDescriptors: Seq[Descriptor],
    rightDescriptors: Seq[Descriptor]) => {
    applyIndividual(
      (x: Descriptor, y: Descriptor) => Matcher.robustCayley(
        x.values[Int], y.values[Int]),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class GeneralizedL0Matcher() extends Matcher {
  import Matcher._
  
  def doMatch = (
    allPairs: Boolean,
    leftDescriptors: Seq[Descriptor],
    rightDescriptors: Seq[Descriptor]) => {
    applyIndividual(
      (x: Descriptor, y: Descriptor) => Matcher.generalizedL0(
        x.values[Int], y.values[Int]),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}