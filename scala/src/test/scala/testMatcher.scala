import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties
import org.scalatest.FunSuite
import nebula.util.Util
import nebula.Matcher._
import nebula.SortDescriptor

class TestMatcher extends FunSuite {
  test("robustCayley") {
    assert(robustCayley(
      IndexedSeq(1, 2, 3),
      IndexedSeq(1, 2, 3)) == 0)

    assert(robustCayley(
      IndexedSeq(1, 1, 2),
      IndexedSeq(2, 1, 1)) == 1)
  }

  test("generalizedL0") {
    assert(generalizedL0(
      IndexedSeq(0, -16),
      IndexedSeq(0, 1)) === 2)

    assert(generalizedL0(
      IndexedSeq(-1, 589828345, -1),
      IndexedSeq(0, -1, 0)) === 2)

    assert(generalizedL0(
      IndexedSeq(10, 1, -10, 1),
      IndexedSeq(0, -10, 0, -10)) === 2)
  }

  test("intervalRanking") {
    assert(
      intervalRanking(
        IndexedSeq(2, 2, 3, 3, 3)).values ===
        IndexedSeq((0, 1), (0, 1), (2, 4), (2, 4), (2, 4)))

    assert(
      intervalRanking(
        IndexedSeq(4, 4, 4)).values ===
        IndexedSeq((0, 2), (0, 2), (0, 2)))

    assert(
      intervalRanking(
        IndexedSeq(4, 4, 2, 4, 5)).values ===
        IndexedSeq((1, 3), (1, 3), (0, 0), (1, 3), (4, 4)))
  }

  test("l1IntervalDistance") {
    assert(
      l1IntervalDistance(
        IndexedSeq(2, 2, 3, 3, 3),
        IndexedSeq(4, 4, 2, 4, 5)) == 0 + 0 + 2 + 0 + 0)

    assert(
      l1IntervalDistance(
        IndexedSeq(1, 2, 3, 4, 4),
        IndexedSeq(5, 4, 3, 2, 1)) == 4 + 2 + 0 + 2 + 3)
  }
}

object CheckMatcher extends Properties("Matcher") {
//  property("generalized l0 returns minimum legal distance") = forAll {
//    (leftLong: List[Int], rightLong: List[Int]) =>
//      {
//        val (left, right) = leftLong.take(6).zip(rightLong).toIndexedSeq.unzip
//        val minimumL0Distance = generalizedL0(left, right)
//
//        val bruteMinimum =
//          (for (
//            (leftPermutation, rightPermutation) <- Util.nonDistinctPermutations(left).zip(Util.nonDistinctPermutations(right))
//          ) yield {
//            l0(
//              SortDescriptor.fromUnsorted(leftPermutation),
//              SortDescriptor.fromUnsorted(rightPermutation))
//          }).min
//
//        minimumL0Distance == bruteMinimum
//      }
//  }
}