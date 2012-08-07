package nebula

import com.googlecode.javacv.cpp.opencv_features2d._

trait MatcherMethod[D <: DescriptorTrait[_]] extends CorrespondenceMethod {
  def apply(allPairs: Boolean, 
	    leftDescriptors: List[D], 
	    rightDescriptors: List[D]): List[DMatch]
}

object MatcherMethod {
  val instances = List(classOf[KendallTauMatcher])
}

case class KendallTauMatcher() extends MatcherMethod[SortDescriptor] {
  def apply(allPairs: Boolean,
	    leftDescriptors: List[SortDescriptor],
	    rightDescriptors: List[SortDescriptor]): List[DMatch] = {
    throw new Exception
  }
}

object Matcher {
  def l0[A](left: DescriptorTrait[A], right: DescriptorTrait[A]): Int = {
    left.values.zip(right.values).count({case (l, r) => l != r})
  }

  def l1(left: DescriptorTrait[Int], right: DescriptorTrait[Int]): Int = {
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
