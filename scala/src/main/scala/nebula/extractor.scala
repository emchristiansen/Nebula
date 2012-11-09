package nebula

import java.awt.image.BufferedImage
import java.awt.image.BufferedImage._
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

import spray.json._
import JSONUtil._
import DenseMatrixUtil._

///////////////////////////////////////////////////////////

sealed trait Extractor extends HasOriginal with JSONSerializable {
  def extract: Extractor.ExtractorAction

  def extractSingle: Extractor.ExtractorActionSingle
}

///////////////////////////////////////////////////////////

object Extractor {
  type ExtractorAction = (BufferedImage, Seq[KeyPoint]) => Seq[Option[Descriptor]]
  type ExtractorActionSingle = (BufferedImage, KeyPoint) => Option[Descriptor]

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

  def doubleExtractorFromEnum(enum: Int): ExtractorActionSingle =
    (image: BufferedImage, keyPoint: KeyPoint) => {
      val extractor = DescriptorExtractor.create(enum)
      val imageMat = OpenCVUtil.bufferedImageToMat(image)
      val descriptor = new Mat
      extractor.compute(imageMat, new MatOfKeyPoint(keyPoint), descriptor)

      if (descriptor.rows == 0 || descriptor.cols == 0) None
      else {
        assert(descriptor.rows == 1)
        assert(descriptor.cols > 0)
//        assert(descriptor.`type` == CvType.CV_8UC1)

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
      for (descriptor <- doubleExtractorFromEnum(enum)(image, keyPoint)) yield {
        descriptor.values[Double].map(_.toInt).flatMap(Util.numToBits(8))
      }
    }

  def intExtractorFromEnum(enum: Int): ExtractorActionSingle =
    (image: BufferedImage, keyPoint: KeyPoint) => {
      for (descriptor <- doubleExtractorFromEnum(enum)(image, keyPoint)) yield {
        descriptor.values[Double].map(_.toInt)
      }
    }

  trait SingleExtractor extends Extractor {
    override def extract = applySeveral(extractSingle)
  }
}

///////////////////////////////////////////////////////////

import Extractor._

///////////////////////////////////////////////////////////

object OpenCVExtractorType extends Enumeration {
  type OpenCVExtractorType = Value
  val BRISK, FREAK, BRIEF, ORB, SIFT, SURF = Value
}

import OpenCVExtractorType.OpenCVExtractorType

case class OpenCVExtractor(extractorType: OpenCVExtractorType)

object OpenCVExtractor {
  import OpenCVExtractorType._

  implicit def implicitOpenCVExtractor(self: OpenCVExtractor): Extractor =
    new SingleExtractor {
      // Doing this one by one is super slow.
      // TODO: Make the native OpenCV api less awful.      
      override def extractSingle = {
        self.extractorType match {
          case BRISK => booleanExtractorFromEnum(DescriptorExtractor.BRISK)
          case FREAK => booleanExtractorFromEnum(DescriptorExtractor.FREAK)
          case BRIEF => booleanExtractorFromEnum(DescriptorExtractor.BRIEF)
          case ORB => booleanExtractorFromEnum(DescriptorExtractor.ORB)
          case SIFT => doubleExtractorFromEnum(DescriptorExtractor.SIFT)
          case SURF => doubleExtractorFromEnum(DescriptorExtractor.SURF)
        }        
      }

      override def original = self

      override def json = JSONUtil.toJSON(self, Nil)
    }
}

///////////////////////////////////////////////////////////    

object PatchExtractorType extends Enumeration {
  type PatchExtractorType = Value
  val Raw, NormalizeRange, NCC, Order, Rank, UniformRank = Value
}

case class PatchExtractor(
  extractorType: PatchExtractorType.PatchExtractorType,
  normalizeRotation: Boolean,
  normalizeScale: Boolean,
  patchWidth: Int,
  blurWidth: Int,
  color: String)

object PatchExtractor {
  import PatchExtractorType._

  def constructor(extractorType: PatchExtractorType): IndexedSeq[Int] => Descriptor = {
    extractorType match {
      case Raw => (raw: IndexedSeq[Int]) => raw
      case NormalizeRange => (raw: IndexedSeq[Int]) => {
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
      case NCC => (raw: IndexedSeq[Int]) => {
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
      case Order => (raw: IndexedSeq[Int]) => {
        SortDescriptor.fromUnsorted(raw)
      }
      case Rank => (raw: IndexedSeq[Int]) => {
        SortDescriptor.fromUnsorted(SortDescriptor.fromUnsorted(raw))
      }
      case UniformRank => (raw: IndexedSeq[Int]) => {
        Extractor.uniformRank(raw)
      }
    }
  }

  implicit def implicitPatchExtractor(self: PatchExtractor): Extractor =
    new SingleExtractor {
      override def extractSingle = (image: BufferedImage, keyPoint: KeyPoint) => {
        val rawOption = rawPixels(
          self.normalizeRotation,
          self.normalizeScale,
          self.patchWidth,
          self.blurWidth,
          self.color)(image, keyPoint)

        for (raw <- rawOption) yield constructor(self.extractorType)(raw)
      }

      override def original = self

      override def json = JSONUtil.toJSON(self, Nil)
    }
}

///////////////////////////////////////////////////////////

case class LogPolarExtractor(
  extractorType: PatchExtractorType.PatchExtractorType,
  normalizeScale: Boolean,
  partitionIntoRings: Boolean,
  minRadius: Double,
  maxRadius: Double,
  numScales: Int,
  numAngles: Int,
  blurWidth: Int,
  color: String)

object LogPolarExtractor {
  import PatchExtractorType._
  import DenseMatrixImplicits._

  implicit def implicitLogPolarExtractor(self: LogPolarExtractor): Extractor =
    new SingleExtractor {
      override def extractSingle = (image: BufferedImage, keyPoint: KeyPoint) => {

        val rawOption = LogPolar.rawLogPolar(
          self.normalizeScale,
          self.minRadius,
          self.maxRadius,
          self.numScales,
          self.numAngles,
          self.blurWidth)(image, keyPoint)

        for (raw <- rawOption) yield {
          val seqSeq = raw.toSeqSeq
          if (self.partitionIntoRings) {
            assert(seqSeq.size == raw.rows)
            assert(seqSeq.head.size == raw.cols)

            val transposed = for (column <- seqSeq.transpose) yield {
              PatchExtractor.constructor(
                self.extractorType)(
                  column).values[Double]
            }
            
            transposed.transpose.toMatrix
          } else {
            val processed =
              PatchExtractor.constructor(
                self.extractorType)(
                  seqSeq.flatten).values[Double]
            processed.grouped(seqSeq.head.size).toIndexedSeq[IndexedSeq[Double]].toMatrix
          }
        }
      }

      override def original = self

      override def json = JSONUtil.toJSON(self, Nil)
    }
}

///////////////////////////////////////////////////////////    

object BRISKExtractorType extends Enumeration {
  type BRISKExtractorType = Value
  val Raw, Order, Rank = Value
}

import BRISKExtractorType.BRISKExtractorType

case class BRISKExtractor(
  extractorType: BRISKExtractorType,
  normalizeRotation: Boolean,
  normalizeScale: Boolean)

object BRISKExtractor {
  import BRISKExtractorType._

  implicit def implicitBRISKExtractor(self: BRISKExtractor): Extractor =
    new SingleExtractor {
      override def extractSingle = (image: BufferedImage, keyPoint: KeyPoint) => {
        val constructor: Descriptor => Descriptor = self.extractorType match {
          case Raw => identity
          case Order => (raw: Descriptor) => {
            SortDescriptor.fromUnsorted(raw.values[Int])
          }
          case Rank => (raw: Descriptor) => {
            SortDescriptor.fromUnsorted(SortDescriptor.fromUnsorted(raw.values[Int]))
          }
        }

        val rawOption = intExtractorFromEnum(DescriptorExtractor.BRISKLUCID)(image, keyPoint)

        for (raw <- rawOption) yield constructor(raw)
      }

      override def original = self

      override def json = JSONUtil.toJSON(self, Nil)
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

      override def original = self

      override def json = JSONUtil.toJSON(self, Nil)
    }
}

///////////////////////////////////////////////////////////

object ExtractorJsonProtocol extends DefaultJsonProtocol {
  implicit val openCVExtractorType = enumeration(
    "OpenCVExtractorType",
    Map(
      "BRISK" -> OpenCVExtractorType.BRISK,
      "FREAK" -> OpenCVExtractorType.FREAK,
      "BRIEF" -> OpenCVExtractorType.BRIEF,
      "ORB" -> OpenCVExtractorType.ORB,
      "SIFT" -> OpenCVExtractorType.SIFT,
      "SURF" -> OpenCVExtractorType.SURF))

  implicit val openCVExtractor =
    jsonFormat1(OpenCVExtractor.apply).addClassInfo("OpenCVExtractor")

  /////////////////////////////////////////////////////////

  implicit val patchExtractorType = enumeration(
    "PatchExtractorType",
    Map(
      "Raw" -> PatchExtractorType.Raw,
      "NormalizeRange" -> PatchExtractorType.NormalizeRange,
      "NCC" -> PatchExtractorType.NCC,
      "Order" -> PatchExtractorType.Order,
      "Rank" -> PatchExtractorType.Rank,
      "UniformRank" -> PatchExtractorType.UniformRank))

  implicit val patchExtractor =
    jsonFormat6(PatchExtractor.apply).addClassInfo("PatchExtractor")

  /////////////////////////////////////////////////////////

  implicit val logPolarExtractor =
    jsonFormat9(LogPolarExtractor.apply).addClassInfo("LogPolarExtractor")

  /////////////////////////////////////////////////////////

  implicit val briskExtractorType = enumeration(
    "BRISKExtractorType",
    Map(
      "Raw" -> BRISKExtractorType.Raw,
      "Order" -> BRISKExtractorType.Order,
      "Rank" -> BRISKExtractorType.Rank))

  implicit val briskExtractor =
    jsonFormat3(BRISKExtractor.apply).addClassInfo("BRISKExtractor")

  /////////////////////////////////////////////////////////

  implicit val elucidExtractor =
    jsonFormat7(ELUCIDExtractor.apply).addClassInfo("ELUCIDExtractor")

  /////////////////////////////////////////////////////////    

  implicit object ExtractorJsonFormat extends RootJsonFormat[Extractor] {
    override def write(self: Extractor) = self.original match {
      case original: OpenCVExtractor => original.toJson
      case original: PatchExtractor => original.toJson
      case original: LogPolarExtractor => original.toJson
      case original: BRISKExtractor => original.toJson
      case original: ELUCIDExtractor => original.toJson
    }
    override def read(value: JsValue) = value.asJsObject.fields("scalaClass") match {
      case JsString("OpenCVExtractor") => value.convertTo[OpenCVExtractor]
      case JsString("PatchExtractor") => value.convertTo[PatchExtractor]
      case JsString("LogPolarExtractor") => value.convertTo[LogPolarExtractor]
      case JsString("BRISKExtractor") => value.convertTo[BRISKExtractor]
      case JsString("ELUCIDExtractor") => value.convertTo[ELUCIDExtractor]
      case _ => throw new DeserializationException("Extractor expected")
    }
  }
}
