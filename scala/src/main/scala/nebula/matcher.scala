package nebula

import org.opencv.features2d.DMatch

import breeze.linalg.DenseMatrix
import graveyard.EpsilonL1Match
import nebula.SortDescriptor.{ implicitIndexedSeq, sortDescriptor }
import nebula.util.JSONUtil._
import spray.json._
import util.JSONUtil.enumeration
import util.Util
import ExtractorJsonProtocol._

///////////////////////////////////////////////////////////

sealed trait Matcher[A] extends HasOriginal {
  def doMatch: Matcher.MatcherAction[A]

  def distance: Matcher.DescriptorDistance[A]
}

object Matcher {
  type MatcherAction[A] = (Boolean, Seq[A], Seq[A]) => Seq[DMatch]
  type DescriptorDistance[A] = (A, A) => Double

  def applyIndividual[A](distanceMethod: DescriptorDistance[A]): MatcherAction[A] =
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

  def apply[A](original: Any, distance: DescriptorDistance[A]): Matcher[A] = new Matcher[A] {
    override def doMatch = applyIndividual(distance)

    override def distance = distance

    override def original = original
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

object MatcherType extends Enumeration {
  import Matcher._

  type MatcherType = Value
  val L0, L1, L2, KendallTau = Value

  // Turn a distance on IndexedSeq[Int] into a distance on SortDescriptor.
  private def lift: DescriptorDistance[IndexedSeq[Int]] => DescriptorDistance[SortDescriptor] =
    distance =>
      (left, right) => distance(left.toIndexedSeq, right.toIndexedSeq)

  implicit def implicitMatcher(self: L0.type) =
    Matcher[IndexedSeq[Any]](self, l0)
  implicit def implicitMatcherSort(self: L0.type) =
    Matcher[SortDescriptor](self, lift(l0))
  implicit def implicitMatcher[A <% Double](self: L1.type) =
    Matcher[IndexedSeq[A]](self, l1)
  implicit def implicitMatcherSort(self: L1.type) =
    Matcher[SortDescriptor](self, lift(l1))
  implicit def implicitMatcher[A <% Double](self: L2.type) =
    Matcher[IndexedSeq[A]](self, l2)
  implicit def implicitMatcherSort(self: L2.type) =
    Matcher[SortDescriptor](self, lift(l2))
  implicit def implictMatcher(self: KendallTau.type) =
    Matcher[SortDescriptor](self, kendallTau)
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
      self,
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
  implicit val matcherType = enumeration(
    "MatcherType",
    Map(
      "L0" -> MatcherType.L0,
      "L1" -> MatcherType.L1,
      "L2" -> MatcherType.L2,
      "KendallTau" -> MatcherType.KendallTau))

  /////////////////////////////////////////////////////////
      
  implicit def logPolarMatcher[A, B](
      implicit a: JsonFormat[Normalizer[DenseMatrix[A], DenseMatrix[B]]],
      b: JsonFormat[Matcher[DenseMatrix[B]]]) =
    jsonFormat5(LogPolarMatcher.apply[A, B]).addClassInfo("LogPolarMatcher")

  /////////////////////////////////////////////////////////      

  implicit def matcherJsonFormat[A] = new RootJsonFormat[Matcher[A]] {
    override def write(self: Matcher[A]) = self.original match {
      case original: MatcherType.MatcherType => original.toJson
      case original: LogPolarMatcher => original.toJson
    }
    override def read(value: JsValue) = {
      value match {
        case JsString(_) => value.convertTo[MatcherType.MatcherType]
        case _ => value.asJsObject.fields("scalaClass") match {
          case JsString("LogPolarMatcher") => {
            value.convertTo[LogPolarMatcher]
          }
          case _ => throw new DeserializationException("Matcher expected")
        }
      }
    }

  }
}
