package nebula

import grizzled.math.stats
import spray.json._
import nebula.util._

///////////////////////////////////////////////////////////    

trait Normalizer[A, B] extends HasOriginal {
  def normalize: A => B
}

object PatchNormalizerType extends Enumeration {
  type PatchNormalizerType = Value
  val Raw, NormalizeRange, NCC, Order, Rank, UniformRank = Value

  implicit class RawNormalize[A](self: Raw.type) extends Normalizer[A, A] {
    override def normalize: A => A = identity
  }

  implicit class RangeNormalizeDouble(self: NormalizeRange.type) extends Normalizer[IndexedSeq[Double], IndexedSeq[Double]] {
    // Sets the value range in [0, 255].
    override def normalize: IndexedSeq[Double] => IndexedSeq[Double] = data => {
      val min = data.min
      val range = data.max - min
      if (range == 0) data // Do nothing.
      else {
        val normalized = data.map(x => ((x - min) * 255.0 / range))
        assert(normalized.min == 0)
        assert(normalized.max == 255)
        normalized
      }
    }
  }

  implicit class NCCNormalizeDouble(self: NCC.type) extends Normalizer[IndexedSeq[Double], IndexedSeq[Double]] {
    override def normalize: IndexedSeq[Double] => IndexedSeq[Double] = data => {
      val mean = stats.mean(data: _*)
      val std = stats.sampleStdDev(data: _*)
      assert(std >= 0)
      val centered = data.map(_ - mean)
      // If the standard deviation is low, merely center the data.  
      if (std < 0.001) centered
      else {
        val normalized = centered.map(_ / std)
        assert(stats.mean(normalized: _*).abs < 0.0001)
        assert((stats.sampleStdDev(normalized: _*) - 1).abs < 0.0001)
        normalized
      }
    }
  }

  implicit class OrderNormalize[A: Ordering](self: Order.type) extends Normalizer[IndexedSeq[A], SortDescriptor] {
    override def normalize: IndexedSeq[A] => SortDescriptor = data => SortDescriptor.fromUnsorted(data)
  }

  implicit class RankNormalize[A: Ordering](self: Rank.type) extends Normalizer[IndexedSeq[A], SortDescriptor] {
    override def normalize: IndexedSeq[A] => SortDescriptor = data => SortDescriptor.fromUnsorted(SortDescriptor.fromUnsorted(data))
  }

  implicit class UniformRankNormalize[A: Ordering](self: UniformRank.type) extends Normalizer[IndexedSeq[A], SortDescriptor] {
    override def normalize: IndexedSeq[A] => IndexedSeq[Int] = data => {
      val distinctPixelValues = data.toSet.toList
      val rank = Rank.normalize(data)
      for (value <- distinctPixelValues) {
        val indices = data.zipWithIndex.filter(_._1 == value).map(_._2)
        val meanRank = (indices.map(rank.apply).sum.toDouble / indices.size).round.toInt
        indices.foreach(i => rank(i) = meanRank)
      }
      rank.toIndexedSeq
    }
  }
  
  implicit class NormalizerFromDoubleToFromInt[A](fromDouble: Normalizer[IndexedSeq[Double], A]) extends Normalizer[IndexedSeq[Int], A] {
    override def normalize: IndexedSeq[Int] => A = data => fromDouble.normalize(data.map(_.toDouble))
  }
  
  def matrixNormalizer(normalizerType: PatchNormalizerType, normalizeRowsSeparately: Boolean) = sys.error("TODO")
}

///////////////////////////////////////////////////////////

object NormalizerJsonProtocol extends DefaultJsonProtocol {
  import PatchNormalizerType._
  
  implicit val patchNormalizerType = JSONUtil.enumeration(
    "PatchNormalizerType",
    Map(
      "Raw" -> PatchNormalizerType.Raw,
      "NormalizeRange" -> PatchNormalizerType.NormalizeRange,
      "NCC" -> PatchNormalizerType.NCC,
      "Order" -> PatchNormalizerType.Order,
      "Rank" -> PatchNormalizerType.Rank,
      "UniformRank" -> PatchNormalizerType.UniformRank))
      
     /////////////////////////////////////////////////////////      
      
  implicit object NormalizerJsonFormatIndexedSeqDoubleIndexedSeqDouble extends RootJsonFormat[Normalizer[IndexedSeq[Double], IndexedSeq[Double]]] {
    override def write(self: Normalizer[IndexedSeq[Double], IndexedSeq[Double]]) = self.original match {
      case original: NormalizeRange.type => original.toJson
      case original: NCC.type => original.toJson
    }
  }
      
  implicit def normalizerJsonFormat[A, B] = new RootJsonFormat[Normalizer[A, B]] {
    override def write(self: Normalizer[A, B]) = self.original match {
      case original: PatchNormalizerType.PatchNormalizerType => original.toJson
    }
    override def read(value: JsValue) = value match {
      case JsString(_) => value.convertTo[PatchNormalizerType.PatchNormalizerType]
    }
  }
}