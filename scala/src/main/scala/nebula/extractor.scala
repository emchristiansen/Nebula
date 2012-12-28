package nebula

import java.awt.image.BufferedImage

import scala.Option.option2Iterable

import org.opencv.core.{ Mat, MatOfKeyPoint }
import org.opencv.features2d.{ DescriptorExtractor, KeyPoint }

import breeze.linalg._
import grizzled.math.stats
//import nebula.Descriptor.implicitIndexedSeq
import nebula.SortDescriptor.implicitIndexedSeq
import nebula.util.DenseMatrixUtil._
import nebula.util.JSONUtil._
import nebula.util.imageProcessing.RichImage.bufferedImage
import spray.json.{ DefaultJsonProtocol, DeserializationException, JsString, JsValue, RootJsonFormat, pimpAny }
import util.{ OpenCVUtil, Util }
import util.JSONUtil.enumeration
import util.imageProcessing.{ ImageUtil, Pixel }
import SortDescriptor._

///////////////////////////////////////////////////////////

sealed trait Extractor[A] extends HasOriginal {
  def extract: Extractor.ExtractorAction[A]

  def extractSingle: Extractor.ExtractorActionSingle[A]
}

///////////////////////////////////////////////////////////

object Extractor {
  type ExtractorAction[A] = (BufferedImage, Seq[KeyPoint]) => Seq[Option[A]]
  type ExtractorActionSingle[A] = (BufferedImage, KeyPoint) => Option[A]

  //  // Computes something like the rank, but pixels with the same value receive
  //  // the same rank, so there is no noise from sort ambiguity.
  //  // This particular algorithm is quite inefficient.
  //  def uniformRank(descriptor: IndexedSeq[Int]): IndexedSeq[Int] = {
  //    val distinctPixelValues = descriptor.toSet.toList
  //    val rank = SortDescriptor.fromUnsorted(SortDescriptor.fromUnsorted(descriptor)).toArray
  //    for (value <- distinctPixelValues) {
  //      val indices = descriptor.zipWithIndex.filter(_._1 == value).map(_._2)
  //      val meanRank = (indices.map(rank.apply).sum.toDouble / indices.size).round.toInt
  //      indices.foreach(i => rank(i) = meanRank)
  //    }
  //    rank.toIndexedSeq
  //  }

  def applySeveral[A](extractSingle: ExtractorActionSingle[A]): ExtractorAction[A] =
    (image: BufferedImage, keyPoints: Seq[KeyPoint]) =>
      keyPoints.map(k => extractSingle(image, k))

  // TODO: These should be enums, not strings.
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

  def doubleExtractorFromEnum(enum: Int): ExtractorActionSingle[IndexedSeq[Double]] =
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

  def intExtractorFromEnum(enum: Int): ExtractorActionSingle[IndexedSeq[Int]] = {
    val toInt: Option[IndexedSeq[Double]] => Option[IndexedSeq[Int]] =
      (seq) => seq.map(_.map(_.round.toInt))
    toInt compose doubleExtractorFromEnum(enum)
  }

  def booleanExtractorFromEnum(enum: Int): ExtractorActionSingle[IndexedSeq[Boolean]] = {
    val toBoolean: Option[IndexedSeq[Int]] => Option[IndexedSeq[Boolean]] =
      (seq) => seq.map(_.flatMap(Util.numToBits(8)))
    toBoolean compose intExtractorFromEnum(enum)
  }

  trait SingleExtractor[A] extends Extractor[A] {
    override def extract = applySeveral(extractSingle)
  }
}

///////////////////////////////////////////////////////////

object OpenCVExtractorType extends Enumeration {
  type OpenCVExtractorType = Value
  val BRISK, FREAK, BRIEF, ORB, SIFT, SURF = Value
}

//sealed trait OpenCVExtractorType
//object BRISK extends OpenCVExtractorType
//object FREAK extends OpenCVExtractorType
//object ORB extends OpenCVExtractorType
//object BRIEF extends OpenCVExtractorType
//object SIFT extends OpenCVExtractorType
//object SURF extends OpenCVExtractorType

//case class OpenCVExtractor(extractorType: OpenCVExtractorType.OpenCVExtractorType)

object OpenCVExtractor {
  import Extractor._
  import OpenCVExtractorType._

  implicit def implicitExtractor(self: BRISK.type) =
    booleanExtractorFromEnum(DescriptorExtractor.BRISK)
  implicit def implicitExtractor(self: FREAK.type) =
    booleanExtractorFromEnum(DescriptorExtractor.FREAK)
  implicit def implicitExtractor(self: BRIEF.type) =
    booleanExtractorFromEnum(DescriptorExtractor.BRIEF)
  implicit def implicitExtractor(self: ORB.type) =
    booleanExtractorFromEnum(DescriptorExtractor.ORB)
  implicit def implicitExtractor(self: SIFT.type) =
    doubleExtractorFromEnum(DescriptorExtractor.SIFT)
  implicit def implicitExtractor(self: SURF.type) =
    doubleExtractorFromEnum(DescriptorExtractor.SURF)

  //  implicit def implicitOpenCVExtractor(self: OpenCVExtractor): Extractor =
  //    new SingleExtractor {
  //      // Doing this one by one is super slow.
  //      // TODO: Make the native OpenCV api less awful.      
  //      override def extractSingle = {
  //        self.extractorType match {
  //          case BRISK => booleanExtractorFromEnum(DescriptorExtractor.BRISK)
  //          case FREAK => booleanExtractorFromEnum(DescriptorExtractor.FREAK)
  //          case BRIEF => booleanExtractorFromEnum(DescriptorExtractor.BRIEF)
  //          case ORB => booleanExtractorFromEnum(DescriptorExtractor.ORB)
  //          case SIFT => doubleExtractorFromEnum(DescriptorExtractor.SIFT)
  //          case SURF => doubleExtractorFromEnum(DescriptorExtractor.SURF)
  //        }
  //      }
  //
  //      override def original = self
  //    }
}




case class PatchExtractor(
//  extractorType: PatchExtractorType.PatchExtractorType,
  normalizeRotation: Boolean,
  normalizeScale: Boolean,
  patchWidth: Int,
  blurWidth: Int,
  color: String)

object PatchExtractor {
  import Extractor._
//  import PatchExtractorType._

  //  // TODO: Duplication with next function.
  //  def constructorDouble(extractorType: PatchExtractorType): IndexedSeq[Double] => Descriptor = {
  //    extractorType match {
  //      case NCC => (raw: IndexedSeq[Double]) => {
  //        val mean = stats.mean(raw: _*)
  //        val std = stats.sampleStdDev(raw: _*)
  //        if (std.abs < 0.001) raw // Don't change it.
  //        else {
  //          val normalized = raw.map(x => (x - mean) / std)
  //          assert(stats.mean(normalized: _*).abs < 0.0001)
  //          assert((stats.sampleStdDev(normalized: _*) - 1).abs < 0.0001)
  //          normalized
  //        }
  //      }
  //
  //      case Rank => (raw: IndexedSeq[Double]) => {
  //        SortDescriptor.fromUnsorted(SortDescriptor.fromUnsorted(raw))
  //      }
  //    }
  //  }
  //
  //  def constructor(extractorType: PatchExtractorType): IndexedSeq[Int] => Descriptor = {
  //    extractorType match {
  //      case Raw => (raw: IndexedSeq[Int]) => raw
  //      case NormalizeRange => (raw: IndexedSeq[Int]) => {
  //        val min = raw.min
  //        val range = raw.max - min
  //        if (range == 0) raw // Do nothing.
  //        else {
  //          val normalized = raw.map(x => ((x - min) * 255.0 / range).round.toInt)
  //          assert(normalized.min == 0)
  //          assert(normalized.max == 255)
  //          normalized
  //        }
  //      }
  //      case NCC => (raw: IndexedSeq[Int]) => {
  //        val mean = stats.mean(raw: _*)
  //        val std = stats.sampleStdDev(raw: _*)
  //        if (std.abs < 0.001) raw // Don't change it.
  //        else {
  //          val normalized = raw.map(x => (x - mean) / std)
  //          assert(stats.mean(normalized: _*).abs < 0.0001)
  //          assert((stats.sampleStdDev(normalized: _*) - 1).abs < 0.0001)
  //          normalized
  //        }
  //      }
  //      case Order => (raw: IndexedSeq[Int]) => {
  //        SortDescriptor.fromUnsorted(raw)
  //      }
  //      case Rank => (raw: IndexedSeq[Int]) => {
  //        SortDescriptor.fromUnsorted(SortDescriptor.fromUnsorted(raw))
  //      }
  //      case UniformRank => (raw: IndexedSeq[Int]) => {
  //        Extractor.uniformRank(raw)
  //      }
  //    }
  //  }

  implicit def implicitPatchExtractor(self: PatchExtractor): Extractor[IndexedSeq[Int]] =
    new SingleExtractor[IndexedSeq[Int]] {
      override def extractSingle = (image: BufferedImage, keyPoint: KeyPoint) => {
        val rawOption = rawPixels(
          self.normalizeRotation,
          self.normalizeScale,
          self.patchWidth,
          self.blurWidth,
          self.color)(image, keyPoint)

          rawOption
//        for (raw <- rawOption) yield constructor(self.extractorType)(raw)
      }

      override def original = self
    }
}

///////////////////////////////////////////////////////////

case class LogPolarExtractor(
//  extractorType: PatchExtractorType.PatchExtractorType,
  steerScale: Boolean,
//  partitionIntoRings: Boolean,
  minRadius: Double,
  maxRadius: Double,
  numScales: Int,
  numAngles: Int,
  blurWidth: Int,
  color: String)

object LogPolarExtractor {
  import Extractor._
//  import PatchExtractorType._
//  import DenseMatrixImplicits._

  implicit def implicitLogPolarExtractor(self: LogPolarExtractor): Extractor[DenseMatrix[Int]] =
    new Extractor[DenseMatrix[Int]] {
      override def extract = (image: BufferedImage, keyPoints: Seq[KeyPoint]) => {
        assert(self.color == "Gray")

        val rawOptions = LogPolar.rawLogPolarSeq(
          self.steerScale,
          self.minRadius,
          self.maxRadius,
          self.numScales,
          self.numAngles,
          self.blurWidth)(image, keyPoints)

        rawOptions
//        for (rawOption <- rawOptions) yield {
//          for (raw <- rawOption) yield {
//            val seqSeq = raw.toSeqSeq
//            if (self.partitionIntoRings) {
//              assert(seqSeq.size == raw.rows)
//              assert(seqSeq.head.size == raw.cols)
//
//              val transposed = for (column <- seqSeq.transpose) yield {
//                PatchExtractor.constructor(
//                  self.extractorType)(
//                    column).values[Double]
//              }
//
//              transposed.transpose.toMatrix.to[Descriptor]
//            } else {
//              val processed = PatchExtractor.constructor(self.extractorType)(
//                seqSeq.flatten).values[Double]
//              processed.grouped(seqSeq.head.size).toIndexedSeq.toMatrix.to[Descriptor]
//            }
//          }
//        }
      }

      override def extractSingle = (image: BufferedImage, keyPoint: KeyPoint) =>
        extract(image, Seq(keyPoint)).head

      override def original = self
    }
}

///////////////////////////////////////////////////////////    

//object BRISKExtractorType extends Enumeration {
//  type BRISKExtractorType = Value
//  val Raw, Order, Rank = Value
//}
//
//case class BRISKExtractor(
//  extractorType: BRISKExtractorType.BRISKExtractorType,
//  normalizeRotation: Boolean,
//  normalizeScale: Boolean)
//
//object BRISKExtractor {
//  import Extractor._
//  import BRISKExtractorType._
//
//  implicit def implicitBRISKExtractor(self: BRISKExtractor): Extractor =
//    new SingleExtractor {
//      override def extractSingle = (image: BufferedImage, keyPoint: KeyPoint) => {
//        val constructor: Descriptor => Descriptor = self.extractorType match {
//          case Raw => identity
//          case Order => (raw: Descriptor) => {
//            SortDescriptor.fromUnsorted(raw.values[Int])
//          }
//          case Rank => (raw: Descriptor) => {
//            SortDescriptor.fromUnsorted(SortDescriptor.fromUnsorted(raw.values[Int]))
//          }
//        }
//
//        val rawOption = intExtractorFromEnum(DescriptorExtractor.BRISKLUCID)(image, keyPoint)
//
//        for (raw <- rawOption) yield constructor(raw)
//      }
//
//      override def original = self
//    }
//}

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
  import Extractor._

  implicit def implicitELUCIDExtractor(self: ELUCIDExtractor): Extractor[SortDescriptor] =
    new SingleExtractor[SortDescriptor] {
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

  implicit val patchExtractor =
    jsonFormat6(PatchExtractor.apply).addClassInfo("PatchExtractor")

  /////////////////////////////////////////////////////////

  implicit val logPolarExtractor =
    jsonFormat9(LogPolarExtractor.apply).addClassInfo("LogPolarExtractor")

  /////////////////////////////////////////////////////////

  //  implicit val briskExtractorType = enumeration(
  //    "BRISKExtractorType",
  //    Map(
  //      "Raw" -> BRISKExtractorType.Raw,
  //      "Order" -> BRISKExtractorType.Order,
  //      "Rank" -> BRISKExtractorType.Rank))

  //  implicit val briskExtractor =
  //    jsonFormat3(BRISKExtractor.apply).addClassInfo("BRISKExtractor")

  /////////////////////////////////////////////////////////

  implicit val elucidExtractor =
    jsonFormat7(ELUCIDExtractor.apply).addClassInfo("ELUCIDExtractor")

  /////////////////////////////////////////////////////////    

  implicit object ExtractorJsonFormat extends RootJsonFormat[Extractor] {
    override def write(self: Extractor) = self.original match {
      case original: OpenCVExtractor => original.toJson
      case original: PatchExtractor => original.toJson
      case original: LogPolarExtractor => original.toJson
      //      case original: BRISKExtractor => original.toJson
      case original: ELUCIDExtractor => original.toJson
    }
    override def read(value: JsValue) = value.asJsObject.fields("scalaClass") match {
      case JsString("OpenCVExtractor") => value.convertTo[OpenCVExtractor]
      case JsString("PatchExtractor") => value.convertTo[PatchExtractor]
      case JsString("LogPolarExtractor") => value.convertTo[LogPolarExtractor]
      //      case JsString("BRISKExtractor") => value.convertTo[BRISKExtractor]
      case JsString("ELUCIDExtractor") => value.convertTo[ELUCIDExtractor]
      case _ => throw new DeserializationException("Extractor expected")
    }
  }
}
