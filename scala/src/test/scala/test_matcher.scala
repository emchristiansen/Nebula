import org.apache.commons.math3.linear.{ Array2DRowRealMatrix, ArrayRealVector }
import org.scalatest.FunSuite

import nebula.KeyPointUtil._

import nebula.{ Homography, KeyPointUtil }

import nebula.Matcher._

import nebula._

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck._

import math._

class TestMatcher extends FunSuite {
  test("robustCayley") {
    assert(robustCayley(
      RawDescriptor(IndexedSeq(1, 2, 3)),
      RawDescriptor(IndexedSeq(1, 2, 3))) == 0)

    assert(robustCayley(
      RawDescriptor(IndexedSeq(1, 1, 2)),
      RawDescriptor(IndexedSeq(2, 1, 1))) == 1)
  }
  
  test("generalizedL0") {
    assert(generalizedL0(
        RawDescriptor(IndexedSeq(0, -16)),
        RawDescriptor(IndexedSeq(0, 1))) === 2)
        
    assert(generalizedL0(
        RawDescriptor(IndexedSeq(-1, 589828345, -1)),
        RawDescriptor(IndexedSeq(0, -1, 0))) === 2)
        
    assert(generalizedL0(
        RawDescriptor(IndexedSeq(10, 1, -10, 1)),
        RawDescriptor(IndexedSeq(0, -10, 0, -10))) === 2)
  }
}

object CheckMatcher extends Properties("Matcher") {
  property("generalized l0 returns minimum legal distance") = forAll {
    (leftLong: List[Int], rightLong: List[Int]) =>
      {
        val (left, right) = leftLong.take(6).zip(rightLong).toIndexedSeq.unzip
        val minimumL0Distance = generalizedL0(RawDescriptor(left), RawDescriptor(right))

        val bruteMinimum =
          (for (
            (leftPermutation, rightPermutation) <- Util.nonDistinctPermutations(left).zip(Util.nonDistinctPermutations(right))
          ) yield {
            l0(
              SortDescriptor.fromUnsorted(leftPermutation),
              SortDescriptor.fromUnsorted(rightPermutation))
          }).min

        minimumL0Distance == bruteMinimum
      }
  }
}