package nebula

import org.opencv.features2d.DMatch

import breeze.linalg.DenseMatrix
import graveyard.EpsilonL1Match
import nebula.SortDescriptor.{ implicitIndexedSeq, sortDescriptor }
import nebula.util.JSONUtil._
import spray.json._
import util.Util

///////////////////////////////////////////////////////////

sealed trait Matcher[F] {
  def doMatch: Matcher.MatcherAction[F]

  def distance: Matcher.DescriptorDistance[F]
}

object Matcher {
  type MatcherAction[F] = (Boolean, Seq[F], Seq[F]) => Seq[DMatch]
  type DescriptorDistance[F] = (F, F) => Double

  def applyIndividual[F](distanceMethod: DescriptorDistance[F]): MatcherAction[F] =
    (allPairs, leftDescriptors, rightDescriptors) => {
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

  def apply[F](descriptorDistance: DescriptorDistance[F]): Matcher[F] = new Matcher[F] {
    override def doMatch = applyIndividual(distance)

    override def distance = descriptorDistance
  }

  def l0(left: IndexedSeq[Any], right: IndexedSeq[Any]): Int =
    (left, right).zipped.count({ case (l, r) => l != r })

  def l1[A <% Double](left: IndexedSeq[A], right: IndexedSeq[A]): Double =
    (left, right).zipped.map({ case (l, r) => (l - r).abs }).sum

  def l2[A <% Double](left: IndexedSeq[A], right: IndexedSeq[A]): Double = {
    math.sqrt((left, right).zipped.map({ case (l, r) => math.pow(l - r, 2) }).sum)
  }

  def kendallTau(left: SortDescriptor, right: SortDescriptor): Int = {
    val size = left.size
    assert(size == right.size)
    val errors = for (i <- 0 until size; j <- i + 1 until size) yield {
      if (left(i) < left(j) == right(i) < right(j)) 0
      else 1
    }
    errors.sum
  }
}

///////////////////////////////////////////////////////////

object MatcherType {
  import Matcher._

  object L0
  object L1
  object L2
  object KendallTau

  // Turn a distance on IndexedSeq[Int] into a distance on SortDescriptor.
  private def lift: DescriptorDistance[IndexedSeq[Int]] => DescriptorDistance[SortDescriptor] =
    distance =>
      (left, right) => distance(left.toIndexedSeq, right.toIndexedSeq)

  // TODO: Why doesn't this work?
//  implicit def implicitMatcher[A](self: L0.type) =
//    Matcher[IndexedSeq[A]](l0)
  implicit def implicitMatcherBoolean(self: L0.type) =
    Matcher[IndexedSeq[Boolean]](l0)
  implicit def implicitMatcherInt(self: L0.type) =
    Matcher[IndexedSeq[Int]](l0)
  implicit def implicitMatcherDouble(self: L0.type) =
    Matcher[IndexedSeq[Double]](l0)     
  implicit def implicitMatcherSort(self: L0.type) =
    Matcher[SortDescriptor](lift(l0))
    
  // TODO: Why doesn't this work?    
  //  implicit def implicitMatcher[A <% Double](self: L1.type) =
  //    Matcher[IndexedSeq[A]](l1)
  implicit def implicitMatcherInt(self: L1.type) =
    Matcher[IndexedSeq[Int]](l1)
  implicit def implicitMatcherDouble(self: L1.type) =
    Matcher[IndexedSeq[Double]](l1)    
  implicit def implicitMatcherSort(self: L1.type) =
    Matcher[SortDescriptor](lift(l1))
    
  // TODO: Why doesn't this work?
  //  implicit def implicitMatcher[A <% Double](self: L2.type): Matcher[IndexedSeq[A]] =
  //    Matcher[IndexedSeq[A]](l2)
  implicit def implicitMatcherInt(self: L2.type) =
    Matcher[IndexedSeq[Int]](l2)    
  implicit def implicitMatcherDouble(self: L2.type) =
    Matcher[IndexedSeq[Double]](l2)
  implicit def implicitMatcherSort(self: L2.type) =
    Matcher[SortDescriptor](lift(l2))
    
  implicit def implictMatcher(self: KendallTau.type) =
    Matcher[SortDescriptor](kendallTau)
}

///////////////////////////////////////////////////////////

case class LogPolarMatcher[A, B](
  normalizer: Normalizer[DenseMatrix[A], DenseMatrix[B]],
  matcher: Matcher[DenseMatrix[B]],
  normalizeByOverlap: Boolean,
  rotationInvariant: Boolean,
  scaleSearchRadius: Int)

import scala.reflect._
import scala.reflect.runtime.universe._

object LogPolarMatcher {
  import Matcher._
  //  import MatcherType._

  implicit def implicitMatcher[A: ClassTag, B](self: LogPolarMatcher[A, B]): Matcher[DenseMatrix[A]] =
    Matcher(
      (left: DenseMatrix[A], right: DenseMatrix[A]) => {
        require(left.rows == right.rows)
        require(left.cols == right.cols)
        require(self.scaleSearchRadius >= 0 && self.scaleSearchRadius < left.cols)

        val distance = self.matcher.distance

        val angleIndices = if (self.rotationInvariant) {
          0 until left.rows
        } else 0 until 1

        val scaleIndices = -self.scaleSearchRadius to self.scaleSearchRadius

        // TODO
        val response = LogPolar.getResponseMap(
          self.normalizer,
          self.normalizeByOverlap,
          self.matcher.distance,
          DenseMatrix.vertcat(left, left(0 until left.rows - 1, ::)),
          right,
          angleIndices,
          scaleIndices)

        response.min
      })
}

///////////////////////////////////////////////////////////

object MatcherJsonProtocol extends DefaultJsonProtocol {
  implicit val l0 = singletonObject(MatcherType.L0)
  implicit val l1 = singletonObject(MatcherType.L1)
  implicit val l2 = singletonObject(MatcherType.L2)
  implicit val kendallTau = singletonObject(MatcherType.KendallTau)  

  /////////////////////////////////////////////////////////

  implicit def logPolarMatcher[A, B](
    implicit a: JsonFormat[Normalizer[DenseMatrix[A], DenseMatrix[B]]],
    b: JsonFormat[Matcher[DenseMatrix[B]]]) =
    jsonFormat5(LogPolarMatcher.apply[A, B]).addClassInfo("LogPolarMatcher")
}
