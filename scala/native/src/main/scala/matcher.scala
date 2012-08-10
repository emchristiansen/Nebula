package nebula

import com.googlecode.javacv.cpp.opencv_features2d._

trait MatcherMethod extends CorrespondenceMethod {
  def apply(allPairs: Boolean, 
	    leftDescriptors: List[SortDescriptor], 
	    rightDescriptors: List[SortDescriptor]): List[DMatch]
}

object MatcherMethod {
  val instances: List[java.lang.Class[_]] = 
    List(classOf[KendallTauMatcher],
         classOf[L0Matcher])
}

case class KendallTauMatcher() extends MatcherMethod {
  def apply(allPairs: Boolean,
	    leftDescriptors: List[SortDescriptor],
	    rightDescriptors: List[SortDescriptor]): List[DMatch] = {
    throw new Exception
  }
}

case class L0Matcher() extends MatcherMethod {
  def apply(allPairs: Boolean,
	    leftDescriptors: List[SortDescriptor],
	    rightDescriptors: List[SortDescriptor]): List[DMatch] = {
    if (allPairs) {
      for ((left, leftIndex) <- leftDescriptors.zipWithIndex;
	   (right, rightIndex) <- rightDescriptors.zipWithIndex) yield {
        val distance = Matcher.l0(left, right)
	new DMatch(leftIndex, rightIndex, distance)
      }
    } else {
      for (((left, right), index) <- leftDescriptors.zip(rightDescriptors).zipWithIndex) yield {
	val distance = Matcher.l0(left, right)
	new DMatch(index, index, distance)
      }
    }
  }  
}

object Matcher {
  def l0[A](left: DescriptorTrait[A], right: DescriptorTrait[A]): Int = {
    left.values.zip(right.values).count({case (l, r) => l != r})
  }

  def l1(left: Descriptor[Int], right: Descriptor[Int]): Int = {
    left.values.zip(right.values).map({case (l, r) => (l - r).abs}).sum
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
}
