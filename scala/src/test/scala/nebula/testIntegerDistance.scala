package nebula

import nebula._
import org.scalatest.FunSuite
import nebula.util.Util
 
///////////////////////////////////////////////////////////

class TestSortDescriptor extends FunSuite {
  val sort0 = SortDescriptor(IndexedSeq(0, 1, 2, 3))
  val sort1 = SortDescriptor(IndexedSeq(2, 1, 3, 0))
  val sort2 = SortDescriptor(IndexedSeq(3, 2, 1, 0))

  test("l0") {
    assert(Matcher.l0(sort0, sort0) === 0)
    assert(Matcher.l0(sort0, sort1) === 3)
  }

  test("l1") {
    assert(Matcher.l1(sort0, sort0) === 0)
    assert(Matcher.l1(sort0, sort1) === 6)
  }

  test("kendallTau") {
    assert(Matcher.kendallTau(sort0, sort0) === 0)
    assert(Matcher.kendallTau(sort0, sort1) === 4)
    assert(Matcher.kendallTau(sort1, sort0) === 4)
    assert(Matcher.kendallTau(sort1, sort1) === 0)
    assert(Matcher.kendallTau(sort0, sort2) === 6)
  }
  
  test("countSort") {
    val input = List(1, 0, 2, 3, 4, 2, 3, 4)
    val countSorted = Util.countSort(input, 0, 4)
    assert(input.sorted == countSorted)
  }
  
  test("permutation") {
    val input = Array(1, 0, 2, 3, 4, 2, 3, 4)
    val permutation = Util.permutation(input, 4)
    assert(permutation === Array(1, 0, 2, 5, 3, 6, 4, 7))
  }

  test("numCycles") {
    assert(sort0.numCycles == 4)
    assert(sort1.numCycles == 2)
    assert(sort2.numCycles == 2)
  }
}
