package nebula

import scala.collection.{ IndexedSeq, Map, Seq }

import org.opencv.features2d.DMatch

import breeze.linalg.DenseMatrix
import graveyard.EpsilonL1Match
import nebula.SortDescriptor.{ implicitIndexedSeq, sortDescriptor }
import nebula.util.JSONUtil._
import spray.json.{ DefaultJsonProtocol, DeserializationException, JsString, JsValue, RootJsonFormat, pimpAny }
import util.JSONUtil.enumeration
import util.Util
import ExtractorJsonProtocol._

///////////////////////////////////////////////////////////

sealed trait Matcher[A] extends HasOriginal {
  def doMatch: Matcher.MatcherAction[A]
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

case class LogPolarMatcher[A <% Double, B <% Double](
  normalizer: Normalizer[DenseMatrix[A], DenseMatrix[B]],    
  matcher: Matcher[DenseMatrix[B]],
  normalizeByOverlap: Boolean,
  rotationInvariant: Boolean,
  scaleSearchRadius: Int)

object LogPolarMatcher {
  import Matcher._
  import MatcherType._

  implicit def implicitMatcher(self: LogPolarMatcher): Matcher = new SingleMatcher {
    override def matchSingle = (left: Descriptor, right: Descriptor) => {
      val leftMatrix = left.original.asInstanceOf[DenseMatrix[Double]]
      val rightMatrix = right.original.asInstanceOf[DenseMatrix[Double]]

      require(leftMatrix.rows == rightMatrix.rows)
      require(leftMatrix.cols == rightMatrix.cols)
      require(self.scaleSearchRadius >= 0 && self.scaleSearchRadius < leftMatrix.cols)

      val distance = LogPolar.getDistance(self.matcherType)

      val angleIndices = if (self.rotationInvariant) {
        0 until leftMatrix.rows
      } else 0 until 1

      val scaleIndices = -self.scaleSearchRadius to self.scaleSearchRadius

      // TODO
      val response = LogPolar.getResponseMap(
        self.normalizer,
        self.normalizeByOverlap,
        distance,
        LogPolar.stackVertical(leftMatrix),
        rightMatrix,
        angleIndices,
        scaleIndices)

      //      val best = response.argmin

      //      println("theta offset is %s".format(best._1 / leftMatrix.rows.toDouble * 2 * math.Pi))

      response.min
    }

    override def original = self
  }
}

///////////////////////////////////////////////////////////

object MatcherJsonProtocol extends DefaultJsonProtocol {
  implicit val matcherType = enumeration(
    "MatcherType",
    Map(
      "L0" -> MatcherType.L0,
      "L1" -> MatcherType.L1,
      "L2" -> MatcherType.L2,
      "KendallTau" -> MatcherType.KendallTau).toMap)

  /////////////////////////////////////////////////////////

  implicit val logPolarMatcher =
    jsonFormat5(LogPolarMatcher.apply).addClassInfo("LogPolarMatcher")

  /////////////////////////////////////////////////////////      

  implicit object MatcherJsonFormat extends RootJsonFormat[Matcher] {
    override def write(self: Matcher) = self.original match {
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
