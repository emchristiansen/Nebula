package nebula

import grizzled.math.stats
import spray.json._
import nebula.util._
import nebula.util.JSONUtil._
import SortDescriptor._
import breeze.linalg._
import reflect._

///////////////////////////////////////////////////////////    

trait Normalizer[-A, +B] {
  def normalize: A => B
}

trait PatchNormalizer {
  object Raw
  object NCC
  object Rank
  object Order
  object NormalizeRange
  object UniformRank
}

object PatchNormalizer extends PatchNormalizer with PatchNormalizerToNormalizer with PatchNormalizerJsonProtocol

///////////////////////////////////////////////////////////

trait PatchNormalizerToNormalizer extends PatchNormalizer {
  implicit class Raw2Normalizer[A](self: Raw.type) extends Normalizer[A, A] {
    override def normalize: A => A = identity
  }

  ///////////////////////////////////////////////////////////  

  implicit class NCC2NormalizerSeq[A <% Double](self: NCC.type) extends Normalizer[Seq[A], IndexedSeq[Double]] {
    override def normalize = data => {
      val doubleData = data.map(_.to[Double])
      val mean = stats.mean(doubleData: _*)
      val std = stats.sampleStdDev(doubleData: _*)
      assert(std >= 0)
      val centered = data.toIndexedSeq.map(_ - mean)
      // If the standard deviation is low, merely center the data.  
      if (std < 0.001) centered
      else {
        val normalized = centered.map(_ / std)
        assertNear(stats.mean(normalized: _*).abs, 0)
        assertNear(stats.sampleStdDev(normalized: _*), 1)
        normalized
      }
    }
  }

  ///////////////////////////////////////////////////////////

  implicit class Rank2NormalizerSeq[A: Ordering](self: Rank.type) extends Normalizer[Seq[A], SortDescriptor] {
    override def normalize = data => SortDescriptor.fromUnsorted(SortDescriptor.fromUnsorted(data))
  }

  ///////////////////////////////////////////////////////////

  implicit class LiftToDenseMatrix[N <% Normalizer[Seq[A], IndexedSeq[B]], A, B: ClassTag](normalizer: N) extends Normalizer[DenseMatrix[A], DenseMatrix[B]] {
    override def normalize = matrix => {
      val normalized = normalizer.normalize(matrix.data)
      new DenseMatrix(matrix.rows, normalized.toArray)
    }
  }

  ///////////////////////////////////////////////////////////

  //  implicit class RangeNormalizeDouble(self: NormalizeRange.type) extends Normalizer[IndexedSeq[Double], IndexedSeq[Double]] {
  //    // Sets the value range in [0, 255].
  //    override def normalize: IndexedSeq[Double] => IndexedSeq[Double] = data => {
  //      val min = data.min
  //      val range = data.max - min
  //      if (range == 0) data // Do nothing.
  //      else {
  //        val normalized = data.map(x => ((x - min) * 255.0 / range))
  //        assert(normalized.min == 0)
  //        assert(normalized.max == 255)
  //        normalized
  //      }
  //    }
  //  }
  //
  //  implicit class OrderNormalize[A: Ordering](self: Order.type) extends Normalizer[IndexedSeq[A], SortDescriptor] {
  //    override def normalize: IndexedSeq[A] => SortDescriptor = data => SortDescriptor.fromUnsorted(data)
  //  }
  //
  //  implicit class UniformRankNormalize[A: Ordering](self: UniformRank.type) extends Normalizer[IndexedSeq[A], IndexedSeq[Int]] {
  //    override def normalize: IndexedSeq[A] => IndexedSeq[Int] = data => {
  //      val distinctPixelValues = data.toSet.toList
  //      // TODO: Fix this
  //      val rank = RankNormalize(Rank).normalize(data).toArray
  //      for (value <- distinctPixelValues) {
  //        val indices = data.zipWithIndex.filter(_._1 == value).map(_._2)
  //        val meanRank = (indices.map(rank.apply).sum.toDouble / indices.size).round.toInt
  //        indices.foreach(i => rank(i) = meanRank)
  //      }
  //      rank.toIndexedSeq
  //    }
  //  }  
}

///////////////////////////////////////////////////////////

trait PatchNormalizerJsonProtocol extends DefaultJsonProtocol {
  implicit val patchNormalizerRawJsonProtocol = singletonObject(PatchNormalizer.Raw)
  implicit val patchNormalizerNormalizeRangeJsonProtocol = singletonObject(PatchNormalizer.NormalizeRange)
  implicit val patchNormalizerNCCJsonProtocol = singletonObject(PatchNormalizer.NCC)
  implicit val patchNormalizerOrderJsonProtocol = singletonObject(PatchNormalizer.Order)
  implicit val patchNormalizerRankJsonProtocol = singletonObject(PatchNormalizer.Rank)
  implicit val patchNormalizerUniformRankJsonProtocol = singletonObject(PatchNormalizer.UniformRank)
}