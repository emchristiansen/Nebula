package nebula

import java.awt.image.BufferedImage
import breeze.math._
import breeze.linalg._
import org.opencv.core._
import org.opencv.features2d._

///////////////////////////////////////////////////////////

case class AffinePair(scale: Double, offset: Double)

case class ScaleMap[A](data: IndexedSeq[A]) {
  require(data.size % 2 == 1)
}

object ScaleMap {
  def apply[A](map: Map[Int, A]): ScaleMap[A] = {
    val minIndex = map.keys.min
    val maxIndex = map.keys.max
    require(minIndex == -maxIndex)
    require(map.keys.toList.sorted == (minIndex to maxIndex))

    val data = map.toList.sortBy(_._1).map(_._2).toIndexedSeq
    ScaleMap(data)
  }
}

case class NCCBlock(fourierData: DenseMatrix[Complex], scaleMap: ScaleMap[AffinePair])

case class NCCLogPolarExtractor(extractor: LogPolarExtractor)

object NCCLogPolarExtractor {
  implicit def nccLogPolarExtractor2Extractor(self: NCCLogPolarExtractor) = {
    val extract = (image: BufferedImage, keyPoint: KeyPoint) => {
      val samplesOption = self.extractor.extractSingle(image, keyPoint)

      for (samples <- samplesOption) yield {
        assert(samples.rows == self.extractor.numScales)
        assert(samples.cols == self.extractor.numAngles)
        
        val zeroPadding = DenseMatrix.zeros[Int](
            self.extractor.numScales - 1, 
            self.extractor.numAngles)
            
        val padded = DenseMatrix.vertcat(samples, zeroPadding)
        
        ???
      }
    }

    Extractor(extract)
  }

  //  implicit def implicitLogPolarExtractor(self: LogPolarExtractor): Extractor[DenseMatrix[Int]] =
  //    new Extractor[DenseMatrix[Int]] {
  //      override def extract = (image: BufferedImage, keyPoints: Seq[KeyPoint]) => {
  //        assert(self.color == "Gray")
  //
  //        LogPolar.rawLogPolarSeq(
  //          self.steerScale,
  //          self.minRadius,
  //          self.maxRadius,
  //          self.numScales,
  //          self.numAngles,
  //          self.blurWidth)(image, keyPoints)
  //
  //        //        for (rawOption <- rawOptions) yield {
  //        //          for (raw <- rawOption) yield {
  //        //            val seqSeq = raw.toSeqSeq
  //        //            if (self.partitionIntoRings) {
  //        //              assert(seqSeq.size == raw.rows)
  //        //              assert(seqSeq.head.size == raw.cols)
  //        //
  //        //              val transposed = for (column <- seqSeq.transpose) yield {
  //        //                PatchExtractor.constructor(
  //        //                  self.extractorType)(
  //        //                    column).values[Double]
  //        //              }
  //        //
  //        //              transposed.transpose.toMatrix.to[Descriptor]
  //        //            } else {
  //        //              val processed = PatchExtractor.constructor(self.extractorType)(
  //        //                seqSeq.flatten).values[Double]
  //        //              processed.grouped(seqSeq.head.size).toIndexedSeq.toMatrix.to[Descriptor]
  //        //            }
  //        //          }
  //        //        }
  //      }
  //
  //      override def extractSingle = (image: BufferedImage, keyPoint: KeyPoint) =>
  //        extract(image, Seq(keyPoint)).head
  //    }
}