import nebula._
import org.scalatest.FunSuite
import javax.imageio.ImageIO
import java.io.File

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util._
import reflect.runtime.universe._
import nebula.util._
import ExpectedDistance._

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
class TestExpectedDistance extends FunSuite {
  def randomSeq(size: Int) = size times { new Random().nextDouble }

  def getDistance[N <% Normalizer[F1, F2], M <% Matcher[F2], F1, F2](
    normalizer: N,
    matcher: M,
    left: F1,
    right: F1): Double = {
    matcher.distance(normalizer.normalize(left), normalizer.normalize(right))
  }

  def getAverageDistance[N <% Normalizer[IndexedSeq[Double], F2], M <% Matcher[F2], F2](
    normalizer: N,
    matcher: M,
    size: Int): Double = {
    val distances = 1000 times {
      val leftSeq = randomSeq(size)
      val rightSeq = randomSeq(size)
      getDistance(normalizer, matcher, leftSeq, rightSeq)
    }

    MathUtil.mean(distances)
  }

  def testExpectedDistance[N <% Normalizer[IndexedSeq[Double], F2], M <% Matcher[F2], F2](
    normalizer: N,
    matcher: M)(implicit ed: ((N, M)) => ExpectedDistance) {
    for (size <- 4 :: 6 :: 10 :: 20 :: 100 :: 200 :: 1000 :: Nil) {
      def near = assertRelativelyNear(1.2) _
      near(
        getAverageDistance(normalizer, matcher, size),
        ed((normalizer, matcher)).expectedDistance(size))
    }
  }

  test("NCC and L1") {
//    val normalizer = PatchNormalizer.NCC
//    val matcher = Matcher.L1
//
//    for (size <- (4 until 10) ++ (10 until 2000 by 10) ++ (2000 until 4200 by 40)) {
//      println(s"${size} ${getAverageDistance(normalizer, matcher, size)}")
//    }
    pending
  }
  
  test("NCC and L2") {
    testExpectedDistance(PatchNormalizer.NCC, Matcher.L2)
  }
  
  test("Rank and L1") {
    testExpectedDistance(PatchNormalizer.Rank, Matcher.L1)
  }
  
  test("Rank and L2") {
    testExpectedDistance(PatchNormalizer.Rank, Matcher.L2)
  }
}