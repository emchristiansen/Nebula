package nebula

import org.opencv.features2d.DMatch

import Util.allSorts

trait MatcherLike[M, D] {
  import MatcherImpl._

  def apply(matcher: M): MatcherAction[D]
}

object MatcherLike {
  val instances: List[java.lang.Class[_]] = List(
    classOf[L0Matcher],
    classOf[L1Matcher],
    classOf[L2Matcher],
    classOf[KendallTauMatcher],
    classOf[CayleyMatcher],
    classOf[CayleyRotate4Matcher],
    classOf[RobustCayleyMatcher],
    classOf[GeneralizedL0Matcher])

  implicit def l0[A] = new MatcherLike[L0Matcher, RawDescriptor[A]] {
    override def apply(matcher: L0Matcher) = matcher.apply[RawDescriptor[A], A]
  }

  // TODO: Can I avoid this extra implicit def?
  implicit def l0Sort = new MatcherLike[L0Matcher, SortDescriptor] {
    override def apply(matcher: L0Matcher) = matcher.apply[SortDescriptor, Int]
  }

  implicit def l1 = new MatcherLike[L1Matcher, RawDescriptor[Int]] {
    override def apply(matcher: L1Matcher) = matcher.apply[RawDescriptor[Int]]
  }

  implicit def l1Sort = new MatcherLike[L1Matcher, SortDescriptor] {
    override def apply(matcher: L1Matcher) = matcher.apply[SortDescriptor]
  }

  implicit def l2 = new MatcherLike[L2Matcher, RawDescriptor[Int]] {
    override def apply(matcher: L2Matcher) = matcher.apply[RawDescriptor[Int]]
  }

  implicit def l2Sort = new MatcherLike[L2Matcher, SortDescriptor] {
    override def apply(matcher: L2Matcher) = matcher.apply[SortDescriptor]
  }

  implicit def kendallTau = new MatcherLike[KendallTauMatcher, SortDescriptor] {
    override def apply(matcher: KendallTauMatcher) = matcher.apply
  }

  implicit def cayley = new MatcherLike[CayleyMatcher, SortDescriptor] {
    override def apply(matcher: CayleyMatcher) = matcher.apply
  }

  implicit def cayleyRotate4 = new MatcherLike[CayleyRotate4Matcher, SortDescriptor] {
    override def apply(matcher: CayleyRotate4Matcher) = matcher.apply
  }

  implicit def robustCayley = new MatcherLike[RobustCayleyMatcher, RawDescriptor[Int]] {
    override def apply(matcher: RobustCayleyMatcher) = matcher.apply
  }
  
  implicit def generalizedL0 = new MatcherLike[GeneralizedL0Matcher, RawDescriptor[Int]] {
    override def apply(matcher: GeneralizedL0Matcher) = matcher.apply
  }  
}

object MatcherImpl {
  type MatcherAction[D] = (Boolean, Seq[D], Seq[D]) => Seq[DMatch]

  def applyIndividual[D](
    distanceMethod: (D, D) => Double,
    allPairs: Boolean,
    leftDescriptors: Seq[D],
    rightDescriptors: Seq[D]): Seq[DMatch] = {
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

sealed trait Matcher

case class L0Matcher() extends Matcher {
  import MatcherImpl._

  def apply[D, E](
    allPairs: Boolean,
    leftDescriptors: Seq[D],
    rightDescriptors: Seq[D])(
      implicit descriptorLike: DescriptorLike[D, E]): Seq[DMatch] = {
    applyIndividual(
      (x: D, y: D) => Matcher.l0(x, y),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class L1Matcher() extends Matcher {
  import MatcherImpl._

  def apply[D](
    allPairs: Boolean,
    leftDescriptors: Seq[D],
    rightDescriptors: Seq[D])(
      implicit descriptorLike: DescriptorLike[D, Int]): Seq[DMatch] = {
    applyIndividual(
      (x: D, y: D) => Matcher.l1(x, y),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class L2Matcher() extends Matcher {
  import MatcherImpl._

  def apply[D](
    allPairs: Boolean,
    leftDescriptors: Seq[D],
    rightDescriptors: Seq[D])(
      implicit descriptorLike: DescriptorLike[D, Int]): Seq[DMatch] = {
    applyIndividual(
      (x: D, y: D) => Matcher.l2(x, y),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class KendallTauMatcher() extends Matcher {
  import MatcherImpl._

  def apply(
    allPairs: Boolean,
    leftDescriptors: Seq[SortDescriptor],
    rightDescriptors: Seq[SortDescriptor]): Seq[DMatch] = {
    applyIndividual(
      Matcher.kendallTau _,
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class CayleyMatcher() extends Matcher {
  import MatcherImpl._

  def apply(
    allPairs: Boolean,
    leftDescriptors: Seq[SortDescriptor],
    rightDescriptors: Seq[SortDescriptor]): Seq[DMatch] = {
    applyIndividual(
      Matcher.cayley _,
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class CayleyRotate4Matcher() extends Matcher {
  import MatcherImpl._

  def apply(
    allPairs: Boolean,
    leftDescriptors: Seq[SortDescriptor],
    rightDescriptors: Seq[SortDescriptor]): Seq[DMatch] = {
    applyIndividual(
      Matcher.cayleyRotate4 _,
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class RobustCayleyMatcher() extends Matcher {
  import MatcherImpl._

  def apply[D](
    allPairs: Boolean,
    leftDescriptors: Seq[D],
    rightDescriptors: Seq[D])(
      implicit descriptorLike: DescriptorLike[D, Int]): Seq[DMatch] = {
    applyIndividual(
      (x: D, y: D) => Matcher.robustCayley(x, y),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class GeneralizedL0Matcher() extends Matcher {
  import MatcherImpl._

  def apply[D](
    allPairs: Boolean,
    leftDescriptors: Seq[D],
    rightDescriptors: Seq[D])(
      implicit descriptorLike: DescriptorLike[D, Int]): Seq[DMatch] = {
    applyIndividual(
      (x: D, y: D) => Matcher.generalizedL0(x, y),
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

import collection._
import nebula.experimental.EpsilonL1Match._

object Matcher {
  def l0[D, E](
    left: D,
    right: D)(
      implicit descriptorLike: DescriptorLike[D, E]): Int = {
    val leftValues = descriptorLike.values(left)
    val rightValues = descriptorLike.values(right)
    leftValues.zip(rightValues).count({ case (l, r) => l != r })
  }

  def l1[D](
    left: D,
    right: D)(
      implicit descriptorLike: DescriptorLike[D, Int]): Int = {
    val leftValues = descriptorLike.values(left)
    val rightValues = descriptorLike.values(right)
    leftValues.zip(rightValues).map({ case (l, r) => (l - r).abs }).sum
  }

  def l2[D](
    left: D,
    right: D)(
      implicit descriptorLike: DescriptorLike[D, Int]): Double = {
    val leftValues = descriptorLike.values(left)
    val rightValues = descriptorLike.values(right)
    math.sqrt(leftValues.zip(rightValues).map({ case (l, r) => math.pow(l - r, 2) }).sum)
  }

  def kendallTau(left: SortDescriptor, right: SortDescriptor): Int = {
    val size = left.values.size
    assert(size == right.values.size)
    val errors = for (i <- 0 until size; j <- i + 1 until size) yield {
      if (left.values(i) < left.values(j) == right.values(i) < right.values(j)) 0
      else 1
    }
    errors.sum
  }

  def cayley(left: SortDescriptor, right: SortDescriptor): Int = {
    assert(left.values.size == right.values.size)
    val rightInverse = SortDescriptor.invert(right)
    val composition = SortDescriptor.compose(left, rightInverse)
    left.values.size - SortDescriptor.numCycles(composition)
  }

  //  def rotate4[D, E](
  //    descriptor: D)(implicit descriptorLike: DescriptorLike[D, E]): Seq[D] = {
  //    val values = descriptorLike.values(descriptor)
  //  }

  // Rotate values taken from a square patch pi / 2 radians.
  // TODO: This "rotate4" idea can be generalized to rotateN on general patches.
  def rotateQuarter[A](descriptor: SortDescriptor): SortDescriptor = {
    // TODO: Doing a color check in this part of the code is clearly dumb.
    val (patchWidth, pixels) = {
      val sqrt = math.sqrt(descriptor.values.size)
      val validGray = sqrt == sqrt.toInt

      val sqrtThird = math.sqrt(descriptor.values.size / 3)
      val validColor = sqrtThird == sqrtThird.toInt

      require(validGray || validColor)
      if (sqrt == sqrt.toInt) (sqrt.toInt, descriptor.values.grouped(1).toIndexedSeq)
      else (sqrtThird.toInt, descriptor.values.grouped(3).toIndexedSeq)
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

  def robustCayley[D](
    left: D,
    right: D)(
      implicit descriptorLike: DescriptorLike[D, Int]): Int = {
    val distances = for (
      leftSort <- allSorts(descriptorLike.values(left));
      rightSort <- allSorts(descriptorLike.values(right))
    ) yield {
      cayley(SortDescriptor(leftSort.toIndexedSeq), SortDescriptor(rightSort.toIndexedSeq))
    }
//    println("distances.size is ", distances.size)
    distances.min
  }
  
//  def pseudoCayley[D](
//      left: D,
//      right: D)(
//        implicit descriptorLike: DescriptorLike[D, Int]): Int = {
//    val leftValues = descriptorLike.values(left)
//    val rightValues = descriptorLike.values(right)
//    
//    def listOfSets(values: IndexedSeq[Int]): IndexedSeq[Set[Int]] = {
//      require(values.min >= 0)
//      require(values.max <= 255)
//      
//      val list = IndexedSeq.fill(256)(mutable.Set[Int]())
//      for ((value, index) <- values.zipWithIndex) list(value) += index
//      list.map(_.toSet)
//    }
//    
//    val leftSets = listOfSets(leftValues)
//    val rightSets = listOfSets(rightValues)
//    
//  }
  
  def generalizedL0[D](
      left: D,
      right: D)(
        implicit descriptorLike: DescriptorLike[D, Int]): Int = {
    val leftValues = descriptorLike.values(left)
    val rightValues = descriptorLike.values(right)
    
    val (leftSorted, rightPermuted) = leftValues.zip(rightValues).sortBy(_._1).unzip
    val groupSizes = Util.group(leftSorted.toList).map(_.size)
    
    val rightPermutedGroups = Util.groupBySizes(groupSizes, rightPermuted)
    val rightSortedGroups = Util.groupBySizes(groupSizes, rightPermuted.sorted)
    
    val permutedHistograms = rightPermutedGroups.map(mkHistogram)
    val sortedHistograms = rightSortedGroups.map(mkHistogram)
    
    permutedHistograms.zip(sortedHistograms).map({ case (l, r) => l1HistogramDistance(l, r) }).sum / 2
  }    
  
  def generalizedCayley[D](
      left: D,
      right: D)(
        implicit descriptorLike: DescriptorLike[D, Int]): Int = {
    val leftValues = descriptorLike.values(left)
    val rightValues = descriptorLike.values(right)
    
    val rightPermuted = leftValues.zip(rightValues).sortBy(_._1).map(_._2)
    Util.numTranspositionsToSort(rightPermuted)
  }
}