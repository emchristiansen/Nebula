package nebula

import com.googlecode.javacv.cpp.opencv_features2d._

trait MatcherMethod extends CorrespondenceMethod {
  protected def applyIndividual(
    distanceMethod: (SortDescriptor, SortDescriptor) => Double,
    allPairs: Boolean,
    leftDescriptors: List[SortDescriptor],
    rightDescriptors: List[SortDescriptor]): List[DMatch] = {
    if (allPairs) {
      for ((left, leftIndex) <- leftDescriptors.zipWithIndex;
	   (right, rightIndex) <- rightDescriptors.zipWithIndex) yield {
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

  def apply(allPairs: Boolean, 
	    leftDescriptors: List[SortDescriptor], 
	    rightDescriptors: List[SortDescriptor]): List[DMatch]
}

object MatcherMethod {
  val instances: List[java.lang.Class[_]] = 
    List(classOf[KendallTauMatcher],
	 classOf[CayleyMatcher],
         classOf[L0Matcher],
	 classOf[L1Matcher],
	 classOf[L2Matcher])
}

case class L0Matcher() extends MatcherMethod {
  def apply(allPairs: Boolean,
	    leftDescriptors: List[SortDescriptor],
	    rightDescriptors: List[SortDescriptor]): List[DMatch] = {
    applyIndividual(
      (x, y) => Matcher.l0[SortDescriptor](x, y).toDouble,
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }  
}

case class L1Matcher() extends MatcherMethod {
  def apply(allPairs: Boolean,
	    leftDescriptors: List[SortDescriptor],
	    rightDescriptors: List[SortDescriptor]): List[DMatch] = {
    applyIndividual(
      (x, y) => Matcher.l1(x, y).toDouble,
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }  
}

case class L2Matcher() extends MatcherMethod {
  def apply(allPairs: Boolean,
	    leftDescriptors: List[SortDescriptor],
	    rightDescriptors: List[SortDescriptor]): List[DMatch] = {
    applyIndividual(
      Matcher.l2 _,
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }  
}

case class KendallTauMatcher() extends MatcherMethod {
  def apply(allPairs: Boolean,
	    leftDescriptors: List[SortDescriptor],
	    rightDescriptors: List[SortDescriptor]): List[DMatch] = {
    applyIndividual(
      Matcher.kendallTau _,
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

case class CayleyMatcher() extends MatcherMethod {
  def apply(allPairs: Boolean,
	    leftDescriptors: List[SortDescriptor],
	    rightDescriptors: List[SortDescriptor]): List[DMatch] = {
    applyIndividual(
      Matcher.cayley _,
      allPairs,
      leftDescriptors,
      rightDescriptors)
  }
}

object Matcher {
  def l0[A <: DescriptorTrait[_]](left: A, right: A): Int = {
    left.values.zip(right.values).count({case (l, r) => l != r})
  }

  def l1(left: SortDescriptor, right: SortDescriptor): Int = {
    left.values.zip(right.values).map({case (l, r) => (l - r).abs}).sum
  }

  def l2(left: SortDescriptor, right: SortDescriptor): Double = {
    math.sqrt(left.values.zip(right.values).map({case (l, r) => math.pow(l - r, 2)}).sum)
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
