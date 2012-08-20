package nebula

import com.googlecode.javacv.cpp.opencv_features2d._

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
    classOf[CayleyMatcher])  

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
}

object MatcherImpl {
  type MatcherAction[D] = (Boolean, Seq[D], Seq[D]) => Seq[DMatch]

  def applyIndividual[D](
    distanceMethod: (D, D) => Double,
    allPairs: Boolean,
    leftDescriptors: Seq[D],
    rightDescriptors: Seq[D]): Seq[DMatch] = {
    if (allPairs) {
      for ((left, leftIndex) <- leftDescriptors.zipWithIndex;
	   (right, rightIndex) <- rightDescriptors.zipWithIndex) yield {
        val distance = distanceMethod(left, right)
	println(left, right, distance)
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

object Matcher {
  def l0[D, E](
    left: D, 
    right: D)(
    implicit descriptorLike: DescriptorLike[D, E]): Int = {
    val leftValues = descriptorLike.values(left)
    val rightValues = descriptorLike.values(right)
    leftValues.zip(rightValues).count({case (l, r) => l != r})
  }

  def l1[D](
    left: D, 
    right: D)(
    implicit descriptorLike: DescriptorLike[D, Int]): Int = {
    val leftValues = descriptorLike.values(left)
    val rightValues = descriptorLike.values(right)
    leftValues.zip(rightValues).map({case (l, r) => (l - r).abs}).sum
  }

  def l2[D](
    left: D, 
    right: D)(
    implicit descriptorLike: DescriptorLike[D, Int]): Double = {
    val leftValues = descriptorLike.values(left)
    val rightValues = descriptorLike.values(right)
    math.sqrt(leftValues.zip(rightValues).map({case (l, r) => math.pow(l - r, 2)}).sum)
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
}
