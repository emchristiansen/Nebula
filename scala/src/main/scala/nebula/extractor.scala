package nebula

import java.awt.image.BufferedImage

import scala.Option.option2Iterable

import org.opencv.core.{ Mat, MatOfKeyPoint }
import org.opencv.features2d.{ DescriptorExtractor, KeyPoint }

import breeze.linalg._
import grizzled.math.stats
import nebula.SortDescriptor.implicitIndexedSeq
import nebula.util.DenseMatrixUtil._
import nebula.util.JSONUtil._
import nebula.util.imageProcessing.RichImage.bufferedImage
import spray.json._
import util.{ OpenCVUtil, Util }
import util.imageProcessing.{ ImageUtil, Pixel }
import SortDescriptor._

import scala.reflect.runtime.universe._

///////////////////////////////////////////////////////////

trait Extractor[F] {
  def extract: Extractor.ExtractorAction[F]

  def extractSingle: Extractor.ExtractorActionSingle[F]
}

///////////////////////////////////////////////////////////

object Extractor {
  type ExtractorAction[F] = (BufferedImage, Seq[KeyPoint]) => Seq[Option[F]]
  type ExtractorActionSingle[F] = (BufferedImage, KeyPoint) => Option[F]

  def fromAction[F](extractSeveral: ExtractorAction[F]) = new Extractor[F] {
    override def extract = extractSeveral

    override def extractSingle = (image, keyPoint) =>
      extract(image, Seq(keyPoint)).head
  }

  def applySeveral[F](extractSingle: ExtractorActionSingle[F]): ExtractorAction[F] =
    (image: BufferedImage, keyPoints: Seq[KeyPoint]) =>
      keyPoints.map(k => extractSingle(image, k))

  def apply[F](single: ExtractorActionSingle[F]): Extractor[F] = new Extractor[F] {
    override def extract = applySeveral(extractSingle)

    override def extractSingle = single
  }

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

  def intExtractorFromEnum(enum: Int): ExtractorActionSingle[IndexedSeq[Int]] = (image, keyPoint) => {
    val toInt: Option[IndexedSeq[Double]] => Option[IndexedSeq[Int]] =
      (seq) => seq.map(_.map(_.round.toInt))
    // TODO: Why doesn't the following work?
    //    toInt compose doubleExtractorFromEnum(enum)
    toInt(doubleExtractorFromEnum(enum)(image, keyPoint))
  }

  def booleanExtractorFromEnum(enum: Int): ExtractorActionSingle[IndexedSeq[Boolean]] = (image, keyPoint) => {
    val toBoolean: Option[IndexedSeq[Int]] => Option[IndexedSeq[Boolean]] =
      (seq) => seq.map(_.flatMap(Util.numToBits(8)))
    // TODO: Why doesn't the following work?      
    //    toBoolean compose intExtractorFromEnum(enum)
    toBoolean(intExtractorFromEnum(enum)(image, keyPoint))
  }
}

///////////////////////////////////////////////////////////

object OpenCVExtractor {
  object BRISK
  object FREAK
  object BRIEF
  object ORB
  object SIFT
  object SURF

  implicit def implicitExtractor(self: BRISK.type) =
    Extractor(Extractor.booleanExtractorFromEnum(DescriptorExtractor.BRISK))
  implicit def implicitExtractor(self: FREAK.type) =
    Extractor(Extractor.booleanExtractorFromEnum(DescriptorExtractor.FREAK))
  implicit def implicitExtractor(self: BRIEF.type) =
    Extractor(Extractor.booleanExtractorFromEnum(DescriptorExtractor.BRIEF))
  implicit def implicitExtractor(self: ORB.type) =
    Extractor(Extractor.booleanExtractorFromEnum(DescriptorExtractor.ORB))
  implicit def implicitExtractor(self: SIFT.type) =
    Extractor(Extractor.doubleExtractorFromEnum(DescriptorExtractor.SIFT))
  implicit def implicitExtractor(self: SURF.type) =
    Extractor(Extractor.doubleExtractorFromEnum(DescriptorExtractor.SURF))
}

case class PatchExtractor(
  normalizeRotation: Boolean,
  normalizeScale: Boolean,
  patchWidth: Int,
  blurWidth: Int,
  color: String)

object PatchExtractor {
  implicit def implicitPatchExtractor(self: PatchExtractor): Extractor[IndexedSeq[Int]] =
    Extractor(
      (image: BufferedImage, keyPoint: KeyPoint) => {
        Extractor.rawPixels(
          self.normalizeRotation,
          self.normalizeScale,
          self.patchWidth,
          self.blurWidth,
          self.color)(image, keyPoint)
      })
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
  implicit def implicitLogPolarExtractor(self: LogPolarExtractor): Extractor[DenseMatrix[Int]] =
    new Extractor[DenseMatrix[Int]] {
      override def extract = (image: BufferedImage, keyPoints: Seq[KeyPoint]) => {
        assert(self.color == "Gray")

        LogPolar.rawLogPolarSeq(
          self.steerScale,
          self.minRadius,
          self.maxRadius,
          self.numScales,
          self.numAngles,
          self.blurWidth)(image, keyPoints)

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
  implicit def implicitELUCIDExtractor(self: ELUCIDExtractor): Extractor[SortDescriptor] =
    Extractor(
      (image: BufferedImage, keyPoint: KeyPoint) => {
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
          val unsorted = pointOptions.flatten.flatMap(Extractor.interpretColor(self.color))
          Some(SortDescriptor.fromUnsorted(unsorted))
        }
      })
}

///////////////////////////////////////////////////////////

case class NormalizedExtractor[E, N, F1, F2](
  extractor: E,
  normalizer: N)(
    implicit evExtractor: E => Extractor[F1],
    evNormalizer: N => Normalizer[F1, F2])

object NormalizedExtractor {
  implicit def toExtractor[E, N, F1, F2](normalizedExtractor: NormalizedExtractor[E, N, F1, F2])(
    implicit evExtractor: E => Extractor[F1],
    evNormalizer: N => Normalizer[F1, F2]): Extractor[F2] = Extractor.fromAction(
    (image: BufferedImage, keyPoints: Seq[KeyPoint]) => {
      val unnormalized = normalizedExtractor.extractor.extract(image, keyPoints)
      unnormalized.map(_.map(normalizedExtractor.normalizer.normalize))
    })
}

///////////////////////////////////////////////////////////

object ExtractorJsonProtocol extends DefaultJsonProtocol {
  implicit val openCVExtractorTypeBrisk = singletonObject(OpenCVExtractor.BRISK)
  implicit val openCVExtractorTypeFreak = singletonObject(OpenCVExtractor.FREAK)
  implicit val openCVExtractorTypeBrief = singletonObject(OpenCVExtractor.BRIEF)
  implicit val openCVExtractorTypeOrb = singletonObject(OpenCVExtractor.ORB)
  implicit val openCVExtractorTypeSift = singletonObject(OpenCVExtractor.SIFT)
  implicit val openCVExtractorTypeSurf = singletonObject(OpenCVExtractor.SURF)

  /////////////////////////////////////////////////////////

  implicit val patchExtractor =
    jsonFormat5(PatchExtractor.apply).addClassInfo("PatchExtractor")

  /////////////////////////////////////////////////////////

  implicit val logPolarExtractor =
    jsonFormat7(LogPolarExtractor.apply).addClassInfo("LogPolarExtractor")

  /////////////////////////////////////////////////////////

  implicit val elucidExtractor =
    jsonFormat7(ELUCIDExtractor.apply).addClassInfo("ELUCIDExtractor")

  /////////////////////////////////////////////////////////    

  implicit def normalizedExtractor[E, N, F1, F2](
    implicit evExtractor: E => Extractor[F1],
    evNormalizer: N => Normalizer[F1, F2],
    evEJson: JsonFormat[E],
    evNJson: JsonFormat[N]) =
    jsonFormat2(NormalizedExtractor.apply[E, N, F1, F2])
}
