import org.apache.commons.math3.linear.{ Array2DRowRealMatrix, ArrayRealVector }
import org.scalatest.FunSuite

import nebula.KeyPointUtil._

import nebula.{ Homography, KeyPointUtil }

import nebula.Matcher._

import nebula._

class TestMatcher extends FunSuite {
  test("robustCayley") {
    assert(robustCayley(
      RawDescriptor(IndexedSeq(1, 2, 3)),
      RawDescriptor(IndexedSeq(1, 2, 3))) == 0)

    assert(robustCayley(
      RawDescriptor(IndexedSeq(1, 1, 2)),
      RawDescriptor(IndexedSeq(2, 1, 1))) == 1)
  }
}