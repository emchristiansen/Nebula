package nebula

import java.awt.image.BufferedImage
import org.opencv.features2d.KeyPoint
import org.opencv.features2d.DescriptorExtractor
import org.opencv.core.Mat
import org.opencv.core.MatOfKeyPoint
import org.opencv.core.CvType
import grizzled.math._

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import util.imageProcessing.RichImage._

import graveyard._
import mpie._
import summary._
import smallBaseline._
import util._
import util.imageProcessing._
import wideBaseline._

///////////////////////////////////////////////////////////

sealed trait Extractor extends JSONSerializable {
  def extract: Extractor.ExtractorAction
}

///////////////////////////////////////////////////////////

object Extractor {
  type ExtractorAction = (BufferedImage, Seq[KeyPoint]) => Seq[Option[Descriptor]]
  type ExtractorActionSingle = (BufferedImage, KeyPoint) => Option[Descriptor]

  val instances: Seq[Class[_]] = nebula.TODO

  // Computes something like the rank, but pixels with the same value receive
  // the same rank, so there is no noise from sort ambiguity.
  // This particular algorithm is quite inefficient.
  def uniformRank(descriptor: IndexedSeq[Int]): IndexedSeq[Int] = {
    val distinctPixelValues = descriptor.toSet.toList
    val rank = SortDescriptor.fromUnsorted(SortDescriptor.fromUnsorted(descriptor)).toArray
    for (value <- distinctPixelValues) {
      val indices = descriptor.zipWithIndex.filter(_._1 == value).map(_._2)
      val meanRank = (indices.map(rank.apply).sum.toDouble / indices.size).round.toInt
      indices.foreach(i => rank(i) = meanRank)
    }
    rank.toIndexedSeq
  }

  def applySeveral(extractSingle: ExtractorActionSingle): ExtractorAction =
    (image: BufferedImage, keyPoints: Seq[KeyPoint]) =>
      keyPoints.map(k => extractSingle(image, k))

  def interpretColor(color: String)(pixel: Pixel): Seq[Int] = color match {
    case "Gray" => pixel.gray
    case "sRGB" => pixel.sRGB
    case "lRGB" => pixel.lRGB
    case "HSB" => pixel.hsb
    case "Lab" => pixel.lab
    case "XYZ" => pixel.xyz
    case _ => sys.error("Color not supported. Do you have a typo?")
  }

  def rawPixels(
    normalizeRotation: Boolean,
    normalizeScale: Boolean,
    patchWidth: Int,
    blurWidth: Int,
    color: String)(
      image: BufferedImage,
      keyPoint: KeyPoint): Option[IndexedSeq[Int]] = {
    // TODO
    assert(!normalizeRotation)
    assert(!normalizeScale)

    val blurred = ImageUtil.boxBlur(blurWidth, image)
    val patchOption = ImageUtil.extractPatch(blurred, patchWidth, keyPoint)
    for (
      patch <- patchOption
    ) yield {
      val values = Pixel.getPixelsOriginal(patch).flatMap(interpretColor(color))
      values
    }
  }

  def extractorFromEnum(enum: Int): ExtractorActionSingle =
    (image: BufferedImage, keyPoint: KeyPoint) => {
      val extractor = DescriptorExtractor.create(enum)
      val imageMat = OpenCVUtil.bufferedImageToMat(image)
      val descriptor = new Mat
      extractor.compute(imageMat, new MatOfKeyPoint(keyPoint), descriptor)

      if (descriptor.rows == 0 || descriptor.cols == 0) None
      else {
        assert(descriptor.rows == 1)
        assert(descriptor.cols > 0)
        assert(descriptor.`type` == CvType.CV_8UC1)

        val doubles = for (c <- 0 until descriptor.cols) yield {
          val doubles = descriptor.get(0, c)
          assert(doubles.size == 1)
          doubles.head
        }

        Some(doubles)
      }
    }

  def booleanExtractorFromEnum(enum: Int): ExtractorActionSingle =
    (image: BufferedImage, keyPoint: KeyPoint) => {
      for (descriptor <- extractorFromEnum(enum)(image, keyPoint)) yield {
        descriptor.values[Int].flatMap(Util.numToBits(8))
      }
    }

  def intExtractorFromEnum(enum: Int): ExtractorActionSingle =
    (image: BufferedImage, keyPoint: KeyPoint) => {
      for (descriptor <- extractorFromEnum(enum)(image, keyPoint)) yield {
        descriptor.values[Int]
      }
    }

  trait SingleExtractor extends Extractor {
    override def extract = applySeveral(extractSingle)

    def extractSingle: ExtractorActionSingle
  }
}

///////////////////////////////////////////////////////////

import Extractor._

///////////////////////////////////////////////////////////

sealed trait OpenCVExtractorType

object OpenCVBRISKExtractor extends OpenCVExtractorType

object OpenCVFREAKExtractor extends OpenCVExtractorType

object OpenCVBRIEFExtractor extends OpenCVExtractorType

object OpenCVORBExtractor extends OpenCVExtractorType

case class OpenCVExtractor(extractorType: OpenCVExtractorType)

object OpenCVExtractor {
  implicit def implicitOpenCVExtractor(self: OpenCVExtractor): Extractor =
    new SingleExtractor {
      // Doing this one by one is super slow.
      // TODO: Make the native OpenCV api less awful.      
      override def extractSingle = {
        val extractorType = self.extractorType match {
          case OpenCVBRISKExtractor => DescriptorExtractor.BRISK
          case OpenCVFREAKExtractor => DescriptorExtractor.FREAK
          case OpenCVBRIEFExtractor => DescriptorExtractor.BRIEF
          case OpenCVORBExtractor => DescriptorExtractor.ORB
        }

        booleanExtractorFromEnum(extractorType)
      }

      override def json = JSONUtil.toJSON(self)
    }
}

///////////////////////////////////////////////////////////

sealed trait PatchExtractorType

object RawExtractor extends PatchExtractorType

object NormalizeRangeExtractor extends PatchExtractorType

object NCCExtractor extends PatchExtractorType

object SortExtractor extends PatchExtractorType

object RankExtractor extends PatchExtractorType

object UniformRankExtractor extends PatchExtractorType

case class PatchExtractor(
  extractorType: PatchExtractorType,
  normalizeRotation: Boolean,
  normalizeScale: Boolean,
  patchWidth: Int,
  blurWidth: Int,
  color: String)

object PatchExtractor {
  implicit def implicitPatchExtractor(self: PatchExtractor): Extractor =
    new SingleExtractor {
      override def extractSingle = (image: BufferedImage, keyPoint: KeyPoint) => {
        val constructor: IndexedSeq[Int] => Descriptor = self.extractorType match {
          case RawExtractor => (raw: IndexedSeq[Int]) => raw
          case NormalizeRangeExtractor => (raw: IndexedSeq[Int]) => {
            val min = raw.min
            val range = raw.max - min
            if (range == 0) raw // Do nothing.
            else {
              val normalized = raw.map(x => ((x - min) * 255.0 / range).round.toInt)
              assert(normalized.min == 0)
              assert(normalized.max == 255)
              normalized
            }
          }
          case NCCExtractor => (raw: IndexedSeq[Int]) => {
            val mean = stats.mean(raw: _*)
            val std = stats.sampleStdDev(raw: _*)
            if (std.abs < 0.001) raw // Don't change it.
            else {
              val normalized = raw.map(x => (x - mean) / std)
              assert(stats.mean(normalized: _*).abs < 0.0001)
              assert((stats.sampleStdDev(normalized: _*) - 1).abs < 0.0001)
              normalized
            }
          }
          case SortExtractor => (raw: IndexedSeq[Int]) => {
            SortDescriptor.fromUnsorted(raw)
          }
          case RankExtractor => (raw: IndexedSeq[Int]) => {
            SortDescriptor.fromUnsorted(SortDescriptor.fromUnsorted(raw))
          }
          case UniformRankExtractor => (raw: IndexedSeq[Int]) => {
            Extractor.uniformRank(raw)
          }
        }

        val rawOption = rawPixels(
          self.normalizeRotation,
          self.normalizeScale,
          self.patchWidth,
          self.blurWidth,
          self.color)(image, keyPoint)

        for (raw <- rawOption) yield constructor(raw)
      }

      override def json = JSONUtil.toJSON(self)
    }
}

///////////////////////////////////////////////////////////    

sealed trait BRISKExtractorType

object BRISKRawExtractor extends BRISKExtractorType

object BRISKOrderExtractor extends BRISKExtractorType

object BRISKRankExtractor extends BRISKExtractorType

case class BRISKExtractor(
  extractorType: BRISKExtractorType,
  normalizeRotation: Boolean,
  normalizeScale: Boolean)

object BRISKExtractor {
  implicit def implicitBRISKExtractor(self: BRISKExtractor): Extractor =
    new SingleExtractor {
      override def extractSingle = (image: BufferedImage, keyPoint: KeyPoint) => {
        val constructor: Descriptor => Descriptor = self.extractorType match {
          case BRISKRawExtractor => identity
          case BRISKOrderExtractor => (raw: Descriptor) => {
            SortDescriptor.fromUnsorted(raw.values[Int])
          }
          case BRISKRankExtractor => (raw: Descriptor) => {
            SortDescriptor.fromUnsorted(SortDescriptor.fromUnsorted(raw.values[Int]))
          }
        }

        val rawOption = intExtractorFromEnum(DescriptorExtractor.BRISKLUCID)(image, keyPoint)

        for (raw <- rawOption) yield constructor(raw)
      }

      override def json = JSONUtil.toJSON(self)
    }
}

///////////////////////////////////////////////////////////      

case class ELUCIDExtractor(
  normalizeRotation: Boolean,
  normalizeScale: Boolean,
  numSamplesPerRadius: Int,
  stepSize: Double,
  numRadii: Int,
  blurWidth: Int,
  color: String)

object ELUCIDExtractor {
  implicit def implicitELUCIDExtractor(self: ELUCIDExtractor): Extractor =
    new SingleExtractor {
      override def extractSingle = (image: BufferedImage, keyPoint: KeyPoint) => {
        val numSamples = self.numRadii * self.numSamplesPerRadius + 1
        val radii = (1 to self.numRadii).map(_ * self.stepSize)
        val angles = (0 until self.numSamplesPerRadius).map(_ * 2 * math.Pi / self.numSamplesPerRadius)

        def samplePattern(scaleFactor: Double, rotationOffset: Double): Seq[DenseVector[Double]] = {
          require(scaleFactor > 0)
          Seq(DenseVector(0.0, 0.0)) ++ (for (
            angle <- angles;
            radius <- radii
          ) yield {
            val scaledRadius = scaleFactor * radius
            val offsetAngle = rotationOffset + angle
            DenseVector(scaledRadius * math.cos(offsetAngle), scaledRadius * math.sin(offsetAngle))
          })
        }

        def samplePoints(keyPoint: KeyPoint): Seq[DenseVector[Double]] = {
          val scaleFactor = if (self.normalizeScale) {
            assert(keyPoint.size > 0)
            keyPoint.size / 10.0
          } else 1

          val rotationOffset = if (self.normalizeRotation) {
            assert(keyPoint.angle != -1)
            keyPoint.angle * 2 * math.Pi / 360
          } else 0

          //    println(rotationOffset)

          samplePattern(scaleFactor, rotationOffset).map(_ + DenseVector(keyPoint.pt.x, keyPoint.pt.y))
        }

        val blurred = ImageUtil.boxBlur(self.blurWidth, image)

        val pointOptions = samplePoints(keyPoint).map(point => blurred.getSubPixel(point(0), point(1)))
        if (pointOptions.contains(None)) None
        else {
          val unsorted = pointOptions.flatten.flatMap(interpretColor(self.color))
          Some(SortDescriptor.fromUnsorted(unsorted))
        }
      }

      override def json = JSONUtil.toJSON(self)
    }
}
