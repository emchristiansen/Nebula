package nebula

import org.opencv.features2d.DMatch

import Util.allSorts

import collection._
import nebula.experimental.EpsilonL1Match._

import PermutationLike.sortDescriptor

sealed trait Matcher {
  import MatcherImpl._

  def doMatch: MatcherAction
}

object MatcherParameterized {
  val instances: List[java.lang.Class[_]] = List(
    classOf[L0Matcher],
    classOf[L1Matcher],
    classOf[L2Matcher],
    classOf[KendallTauMatcher],
    classOf[CayleyMatcher],
    classOf[CayleyRotate4Matcher],
    classOf[RobustCayleyMatcher],
    classOf[GeneralizedL0Matcher])

  def l0(left: IndexedSeq[Any], right: IndexedSeq[Any]): Int =
    (left, right).zipped.count({ case (l, r) => l != r })

  def l1(left: IndexedSeq[Int], right: IndexedSeq[Int]): Int =
    (left, right).zipped.map({ case (l, r) => (l - r).abs }).sum

  def l2(left: IndexedSeq[Int], right: IndexedSeq[Int]): Double = {
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
      leftSort <- allSorts(left);
      rightSort <- allSorts(right)
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

    val permutedHistograms = rightPermutedGroups.map(mkHistogram)
    val sortedHistograms = rightSortedGroups.map(mkHistogram)

    permutedHistograms.zip(sortedHistograms).map({ case (l, r) => l1HistogramDistance(l, r) }).sum / 2
  }

  def generalizedCayley(left: IndexedSeq[Int], right: IndexedSeq[Int]): Int = {
    val rightPermuted = left.zip(right).sortBy(_._1).map(_._2)
    Util.numTranspositionsToSort(rightPermuted)
  }
}

/////////////////////////////////////////////////////////////////////////////

object MatcherImpl {
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
}

case class L0Matcher() extends Matcher {
  import MatcherImpl._

  override def doMatch = (
    allPairs: Boolean,
    leftDescriptors: Seq[Descriptor],
    rightDescriptors: Seq[Descriptor]) => {
    applyIndividual(
      (x: Descriptor, y: Descriptor) =>
        MatcherParameterized.l0(
          x.values[Any],
          y.values[Any]),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class L1Matcher() extends Matcher {
  import MatcherImpl._

  def doMatch = (
    allPairs: Boolean,
    leftDescriptors: Seq[Descriptor],
    rightDescriptors: Seq[Descriptor]) => {
    applyIndividual(
      (x: Descriptor, y: Descriptor) =>
        MatcherParameterized.l1(
          x.values[Int],
          y.values[Int]),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class L2Matcher() extends Matcher {
  import MatcherImpl._

  def doMatch = (
    allPairs: Boolean,
    leftDescriptors: Seq[Descriptor],
    rightDescriptors: Seq[Descriptor]) => {
    applyIndividual(
      (x: Descriptor, y: Descriptor) =>
        MatcherParameterized.l2(
          x.values[Int],
          y.values[Int]),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class KendallTauMatcher() extends Matcher {
  import MatcherImpl._

  def doMatch = (
    allPairs: Boolean,
    leftDescriptors: Seq[Descriptor],
    rightDescriptors: Seq[Descriptor]) => {
    applyIndividual(
      (x: Descriptor, y: Descriptor) => MatcherParameterized.kendallTau(
        x.asType[SortDescriptor], y.asType[SortDescriptor]),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class CayleyMatcher() extends Matcher {
  import MatcherImpl._

  def doMatch = (
    allPairs: Boolean,
    leftDescriptors: Seq[Descriptor],
    rightDescriptors: Seq[Descriptor]) => {
    applyIndividual(
      (x: Descriptor, y: Descriptor) => MatcherParameterized.cayley(
        x.asType[SortDescriptor], y.asType[SortDescriptor]),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class CayleyRotate4Matcher() extends Matcher {
  import MatcherImpl._

  def doMatch = (
    allPairs: Boolean,
    leftDescriptors: Seq[Descriptor],
    rightDescriptors: Seq[Descriptor]) => {
    applyIndividual(
      (x: Descriptor, y: Descriptor) => MatcherParameterized.cayleyRotate4(
        x.asType[SortDescriptor], y.asType[SortDescriptor]),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class RobustCayleyMatcher() extends Matcher {
  import MatcherImpl._

  def doMatch = (
    allPairs: Boolean,
    leftDescriptors: Seq[Descriptor],
    rightDescriptors: Seq[Descriptor]) => {
    applyIndividual(
      (x: Descriptor, y: Descriptor) => MatcherParameterized.robustCayley(
        x.values[Int], y.values[Int]),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class GeneralizedL0Matcher() extends Matcher {
  import MatcherImpl._

  def doMatch = (
    allPairs: Boolean,
    leftDescriptors: Seq[Descriptor],
    rightDescriptors: Seq[Descriptor]) => {
    applyIndividual(
      (x: Descriptor, y: Descriptor) => MatcherParameterized.generalizedL0(
        x.values[Int], y.values[Int]),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}
