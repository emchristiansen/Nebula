package nebula

import scala.collection.{ IndexedSeq, Map, Seq }

import org.opencv.features2d.DMatch

import breeze.linalg.DenseMatrix
import graveyard.EpsilonL1Match
import nebula.SortDescriptor.{ implicitIndexedSeq, sortDescriptor }
import nebula.util.JSONUtil.implicitAddClassName
import spray.json.{ DefaultJsonProtocol, DeserializationException, JsString, JsValue, RootJsonFormat, pimpAny }
import util.JSONUtil.enumeration
import util.Util
import ExtractorJsonProtocol._

///////////////////////////////////////////////////////////

sealed trait Matcher extends HasOriginal {
  def doMatch: Matcher.MatcherAction
}

object Matcher {
  type MatcherAction = (Boolean, Seq[Descriptor], Seq[Descriptor]) => Seq[DMatch]
  type DescriptorDistance = (Descriptor, Descriptor) => Double

  def applyIndividual(distanceMethod: DescriptorDistance): MatcherAction =
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

  def cayley(left: SortDescriptor, right: SortDescriptor): Int = {
    assert(left.size == right.size)

    val rightInverse = right.invert
    val composition = left.compose(rightInverse)
    left.size - composition.numCycles
  }

  // Rotate values taken from a square patch pi / 2 radians.
  // TODO: This "rotate4" idea can be generalized to rotateN on general patches.
  def rotateQuarter(descriptor: SortDescriptor): SortDescriptor = {
    // TODO: Doing a color check in this part of the code is clearly dumb.
    val (patchWidth, pixels) = {
      val sqrt = math.sqrt(descriptor.size)
      val validGray = sqrt == sqrt.toInt

      val sqrtThird = math.sqrt(descriptor.size / 3)
      val validColor = sqrtThird == sqrtThird.toInt

      require(validGray || validColor)
      if (sqrt == sqrt.toInt) (sqrt.toInt, descriptor.grouped(1).toIndexedSeq)
      else (sqrtThird.toInt, descriptor.grouped(3).toIndexedSeq)
    }

    val indices = (0 until pixels.size).grouped(patchWidth).toSeq
    val rotated = indices.transpose.map(_.reverse)
    SortDescriptor(rotated.flatten.map(i => pixels(i)).flatten.toIndexedSeq)
  }

  def rotate4(descriptor: SortDescriptor): Seq[SortDescriptor] = {
    lazy val rotaters: Stream[SortDescriptor => SortDescriptor] =
      Stream.cons(identity, rotaters.map(f => rotateQuarter _ compose f))
    rotaters.map(_(descriptor)).take(4).toList
  }

  def cayleyRotate4(left: SortDescriptor, right: SortDescriptor): Int = {
    val rightRotations = rotate4(right)
    val distances = rightRotations.map(r => cayley(left, r))
    distances.min
  }

  def robustCayley(left: IndexedSeq[Int], right: IndexedSeq[Int]): Int = {
    val distances = for (
      leftSort <- Util.allSorts(left);
      rightSort <- Util.allSorts(right)
    ) yield {
      cayley(SortDescriptor(leftSort.toIndexedSeq), SortDescriptor(rightSort.toIndexedSeq))
    }
    distances.min
  }

  def generalizedL0(left: IndexedSeq[Int], right: IndexedSeq[Int]): Int = {
    val (leftSorted, rightPermuted) = left.zip(right).sortBy(_._1).unzip
    val groupSizes = Util.group(leftSorted.toList).map(_.size)

    val rightPermutedGroups = Util.groupBySizes(groupSizes, rightPermuted)
    val rightSortedGroups = Util.groupBySizes(groupSizes, rightPermuted.sorted)

    val permutedHistograms = rightPermutedGroups.map(EpsilonL1Match.mkHistogram)
    val sortedHistograms = rightSortedGroups.map(EpsilonL1Match.mkHistogram)

    permutedHistograms.zip(sortedHistograms).map({ case (l, r) => EpsilonL1Match.l1HistogramDistance(l, r) }).sum / 2
  }

  def generalizedCayley(left: IndexedSeq[Int], right: IndexedSeq[Int]): Int = {
    val rightPermuted = left.zip(right).sortBy(_._1).map(_._2)
    Util.numTranspositionsToSort(rightPermuted)
  }

  ///////////////////////////////////////////////////////////  

  case class IntervalRanking(values: IndexedSeq[Tuple2[Int, Int]]) {
    values.foreach(t => assert(t._1 <= t._2))
  }

  def intervalRanking(descriptor: IndexedSeq[Int]): IntervalRanking = {
    val distinctPixelValues = descriptor.toSet.toList
    val rank = SortDescriptor.fromUnsorted(SortDescriptor.fromUnsorted(descriptor))
    val intervals = Array.fill(rank.size)((-1, -1))
    for (value <- distinctPixelValues) {
      val indices = descriptor.zipWithIndex.filter(_._1 == value).map(_._2)
      val minRank = indices.map(rank.apply).min
      val maxRank = indices.map(rank.apply).max
      indices.foreach(i => intervals(i) = (minRank, maxRank))
    }
    IntervalRanking(intervals.toIndexedSeq)
  }

  def l1IntervalDistance(left: IndexedSeq[Int], right: IndexedSeq[Int]): Int = {
    val leftIntervals = intervalRanking(left)
    val rightIntervals = intervalRanking(right)

    def l1Distance(leftInterval: Tuple2[Int, Int], rightInterval: Tuple2[Int, Int]): Int = {
      // Dumb but def correct.
      (for (
        l <- leftInterval._1 to leftInterval._2;
        r <- rightInterval._1 to rightInterval._2
      ) yield math.abs(l - r)).min
    }

    leftIntervals.values.zip(rightIntervals.values).map({ case (l, r) => l1Distance(l, r) }).sum
  }

  trait SingleMatcher extends Matcher {
    override def doMatch = applyIndividual(matchSingle)

    def matchSingle: DescriptorDistance
  }
}

///////////////////////////////////////////////////////////

object MatcherType extends Enumeration {
  import Matcher._

  type MatcherType = Value
  val L0, L1, L1Interval, L2, KendallTau, Cayley, CayleyRotate4, RobustCayley, GeneralizedL0 = Value

  implicit def implicitMatcher(self: MatcherType): Matcher = new SingleMatcher {
    override def matchSingle = {
      def cast[A: Manifest](distance: (IndexedSeq[A], IndexedSeq[A]) => Double) =
        (left: Descriptor, right: Descriptor) =>
          distance(left.values[A], right.values[A])

      def sort(distance: (SortDescriptor, SortDescriptor) => Double) =
        (left: Descriptor, right: Descriptor) => (left, right) match {
          case (left: SortDescriptor, right: SortDescriptor) => distance(left, right)
          case _ => sys.error("Expected SortDescriptor")
        }

      self match {
        case L0 => cast[Any](l0)
        case L1 => cast[Double](l1)
        case L1Interval => cast[Int](l1IntervalDistance)
        case L2 => cast[Double](l2)
        case KendallTau => sort(kendallTau)
        case Cayley => sort(cayley)
        case CayleyRotate4 => sort(cayleyRotate4)
        case RobustCayley => cast[Int](robustCayley)
        case GeneralizedL0 => cast[Int](generalizedL0)
      }
    }

    override def original = self
  }
}

///////////////////////////////////////////////////////////

case class LogPolarMatcher(
  matcherType: MatcherType.MatcherType,
  normalization: PatchExtractorType.PatchExtractorType,
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
        self.normalization,
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
      "L1Interval" -> MatcherType.L1Interval,
      "L2" -> MatcherType.L2,
      "KendallTau" -> MatcherType.KendallTau,
      "Cayley" -> MatcherType.Cayley,
      "CayleyRotate4" -> MatcherType.CayleyRotate4,
      "RobustCayley" -> MatcherType.RobustCayley,
      "GeneralizedL0" -> MatcherType.GeneralizedL0).toMap)

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
