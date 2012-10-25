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

sealed trait Extractor {
  import Extractor._

  def extract: ExtractorAction
}

///////////////////////////////////////////////////////////

object Extractor {
  type ExtractorAction = (BufferedImage, Seq[KeyPoint]) => Seq[Option[Descriptor]]
  type ExtractorActionSingle = (BufferedImage, KeyPoint) => Option[Descriptor]

  val instances: Seq[Class[_]] = Seq(
    classOf[RawExtractor],
    classOf[NormalizeExtractor],
    classOf[NCCExtractor],
    classOf[SortExtractor],
    classOf[RankExtractor],
    classOf[UniformRankExtractor],
    classOf[BRISKExtractor],
    classOf[BRISKRawExtractor],
    classOf[BRISKRankExtractor],
    classOf[BRISKOrderExtractor],
    classOf[FREAKExtractor],
    classOf[BRIEFExtractor],
    classOf[ORBExtractor],
    classOf[ELUCIDExtractor])

  // Computes something like the rank, but pixels with the same value receive
  // the same rank, so there is no noise from sort ambiguity.
  // This particular algorithm is quite inefficient.
  def uniformRank(descriptor: RawDescriptor[Int]): RawDescriptor[Int] = {
    val distinctPixelValues = descriptor.values.toSet.toList
    val rank = SortDescriptor.fromUnsorted(SortDescriptor.fromUnsorted(descriptor.values)).toArray
    for (value <- distinctPixelValues) {
      val indices = descriptor.values.zipWithIndex.filter(_._1 == value).map(_._2)
      val meanRank = (indices.map(rank.apply).sum.toDouble / indices.size).round.toInt
      indices.foreach(i => rank(i) = meanRank)
    }
    RawDescriptor(rank.toIndexedSeq)
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
      keyPoint: KeyPoint): Option[RawDescriptor[Int]] = {
    // TODO
    assert(!normalizeRotation)
    assert(!normalizeScale)

    val blurred = ImageUtil.boxBlur(blurWidth, image)
    val patchOption = ImageUtil.extractPatch(blurred, patchWidth, keyPoint)
    for (
      patch <- patchOption
    ) yield {
      val values = Pixel.getPixelsOriginal(patch).flatMap(interpretColor(color))
      RawDescriptor(values)
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

        Some(RawDescriptor(doubles))
      }
    }

  def booleanExtractorFromEnum(enum: Int): ExtractorActionSingle =
    (image: BufferedImage, keyPoint: KeyPoint) => {
      for (descriptor <- extractorFromEnum(enum)(image, keyPoint)) yield {
        val doubles = descriptor.values[Double]
        RawDescriptor(doubles.map(_.toInt).flatMap(Util.numToBits(8)))
      }
    }

  def intExtractorFromEnum(enum: Int): ExtractorActionSingle =
    (image: BufferedImage, keyPoint: KeyPoint) => {
      for (descriptor <- extractorFromEnum(enum)(image, keyPoint)) yield {
        val doubles = descriptor.values[Double]
        RawDescriptor(doubles.map(_.toInt))
      }
    }
}

///////////////////////////////////////////////////////////

// TODO: A lot of duplicated code in here.

case class RawExtractor(
  normalizeRotation: Boolean,
  normalizeScale: Boolean,
  patchWidth: Int,
  blurWidth: Int,
  color: String) extends Extractor {
  import Extractor._

  def extract = applySeveral(extractSingle)

  // TODO: It's dumb I have to pass these parameters explicitly.
  def extractSingle: ExtractorActionSingle = rawPixels(
    normalizeRotation,
    normalizeScale,
    patchWidth,
    blurWidth,
    color)
}

case class NormalizeExtractor(
  normalizeRotation: Boolean,
  normalizeScale: Boolean,
  patchWidth: Int,
  blurWidth: Int,
  color: String) extends Extractor {
  import Extractor._

  def extract = applySeveral(extractSingle)

  def extractSingle: ExtractorActionSingle =
    (image: BufferedImage, keyPoint: KeyPoint) => {
      val rawOption = rawPixels(
        normalizeRotation,
        normalizeScale,
        patchWidth,
        blurWidth,
        color)(image, keyPoint)
      for (raw <- rawOption) yield {
        val values = raw.values
        val min = values.min
        val range = values.max - min
        if (range == 0) RawDescriptor(raw) // Do nothing.
        else {
          val normalized = values.map(x => ((x - min) * 255.0 / range).round.toInt)
          assert(normalized.min == 0)
          assert(normalized.max == 255)
          RawDescriptor(normalized)
        }
      }
    }
}

case class NCCExtractor(
  normalizeRotation: Boolean,
  normalizeScale: Boolean,
  patchWidth: Int,
  blurWidth: Int,
  color: String) extends Extractor {
  import Extractor._

  def extract = applySeveral(extractSingle)

  def extractSingle: ExtractorActionSingle =
    (image: BufferedImage, keyPoint: KeyPoint) => {
      val rawOption = rawPixels(
        normalizeRotation,
        normalizeScale,
        patchWidth,
        blurWidth,
        color)(image, keyPoint)
      for (raw <- rawOption) yield {
        val values = raw.values
        val mean = stats.mean(values: _*)
        val std = stats.sampleStdDev(values: _*)
        if (std.abs < 0.001) RawDescriptor(raw) // Don't change it.
        else {
          val normalized = values.map(x => (x - mean) / std)
          assert(stats.mean(normalized: _*).abs < 0.0001)
          assert((stats.sampleStdDev(normalized: _*) - 1).abs < 0.0001)
          RawDescriptor(normalized)
        }
      }
    }
}

case class SortExtractor(
  normalizeRotation: Boolean,
  normalizeScale: Boolean,
  patchWidth: Int,
  blurWidth: Int,
  color: String) extends Extractor {
  import Extractor._

  def extract = applySeveral(extractSingle)

  def extractSingle: ExtractorActionSingle =
    (image: BufferedImage, keyPoint: KeyPoint) => {
      val unsortedOption = rawPixels(
        normalizeRotation,
        normalizeScale,
        patchWidth,
        blurWidth,
        color)(image, keyPoint)
      for (unsorted <- unsortedOption) yield {
        SortDescriptor.fromUnsorted(unsorted.values)
      }
    }
}

case class RankExtractor(
  normalizeRotation: Boolean,
  normalizeScale: Boolean,
  patchWidth: Int,
  blurWidth: Int,
  color: String) extends Extractor {
  import Extractor._

  def extract = applySeveral(extractSingle)

  def extractSingle: ExtractorActionSingle =
    (image: BufferedImage, keyPoint: KeyPoint) => {
      val unsortedOption = rawPixels(
        normalizeRotation,
        normalizeScale,
        patchWidth,
        blurWidth,
        color)(image, keyPoint)
      for (unsorted <- unsortedOption) yield {
        SortDescriptor.fromUnsorted(SortDescriptor.fromUnsorted(unsorted.values))
      }
    }
}

case class UniformRankExtractor(
  normalizeRotation: Boolean,
  normalizeScale: Boolean,
  patchWidth: Int,
  blurWidth: Int,
  color: String) extends Extractor {
  import Extractor._

  def extract = applySeveral(extractSingle)

  def extractSingle: ExtractorActionSingle =
    (image: BufferedImage, keyPoint: KeyPoint) => {
      val unsortedOption = rawPixels(
        normalizeRotation,
        normalizeScale,
        patchWidth,
        blurWidth,
        color)(image, keyPoint)
      for (unsorted <- unsortedOption) yield {
        Extractor.uniformRank(unsorted)
      }
    }
}

case class BRISKExtractor(
  normalizeRotation: Boolean,
  normalizeScale: Boolean) extends Extractor {
  import Extractor._

  def extract = applySeveral(extractSingle)

  // Doing this one by one is super slow.
  // TODO: Make the native OpenCV api less awful.
  def extractSingle: ExtractorActionSingle =
    booleanExtractorFromEnum(DescriptorExtractor.BRISK)
}

case class BRISKRawExtractor(
  normalizeRotation: Boolean,
  normalizeScale: Boolean) extends Extractor {
  import Extractor._

  def extract = applySeveral(extractSingle)

  // Doing this one by one is super slow.
  // TODO: Make the native OpenCV api less awful.
  def extractSingle: ExtractorActionSingle = //sys.error("TODO")
    intExtractorFromEnum(DescriptorExtractor.BRISKLUCID)
}

case class BRISKOrderExtractor(
  normalizeRotation: Boolean,
  normalizeScale: Boolean) extends Extractor {
  import Extractor._

  def extract = applySeveral(extractSingle)

  // Doing this one by one is super slow.
  // TODO: Make the native OpenCV api less awful.
  def extractSingle: ExtractorActionSingle = (image, keyPoint) => {
    //    sys.error("TODO")
    for (descriptor <- intExtractorFromEnum(DescriptorExtractor.BRISKLUCID)(image, keyPoint)) yield SortDescriptor.fromUnsorted(descriptor.values[Int])
  }
}

case class BRISKRankExtractor(
  normalizeRotation: Boolean,
  normalizeScale: Boolean) extends Extractor {
  import Extractor._

  def extract = applySeveral(extractSingle)

  // Doing this one by one is super slow.
  // TODO: Make the native OpenCV api less awful.
  def extractSingle: ExtractorActionSingle = (image, keyPoint) => {
    //    sys.error("TODO")
    for (descriptor <- intExtractorFromEnum(DescriptorExtractor.BRISKLUCID)(image, keyPoint)) yield SortDescriptor.fromUnsorted(SortDescriptor.fromUnsorted(descriptor.values[Int]))
  }
}

case class FREAKExtractor(
  normalizeRotation: Boolean,
  normalizeScale: Boolean) extends Extractor {
  import Extractor._

  def extract = applySeveral(extractSingle)

  // Doing this one by one is super slow.
  // TODO: Make the native OpenCV api less awful.
  def extractSingle: ExtractorActionSingle =
    booleanExtractorFromEnum(DescriptorExtractor.FREAK)
}

case class BRIEFExtractor(
  normalizeRotation: Boolean,
  normalizeScale: Boolean) extends Extractor {
  import Extractor._

  def extract = applySeveral(extractSingle)

  // Doing this one by one is super slow.
  // TODO: Make the native OpenCV api less awful.
  def extractSingle: ExtractorActionSingle =
    booleanExtractorFromEnum(DescriptorExtractor.BRIEF)
}

case class ORBExtractor(
  normalizeRotation: Boolean,
  normalizeScale: Boolean) extends Extractor {
  import Extractor._

  def extract = applySeveral(extractSingle)

  // Doing this one by one is super slow.
  // TODO: Make the native OpenCV api less awful.
  def extractSingle: ExtractorActionSingle =
    booleanExtractorFromEnum(DescriptorExtractor.ORB)
}

case class ELUCIDExtractor(
  normalizeRotation: Boolean,
  normalizeScale: Boolean,
  numSamplesPerRadius: Int,
  stepSize: Double,
  numRadii: Int,
  blurWidth: Int,
  color: String) extends Extractor {
  import Extractor._

  def extract = applySeveral(extractSingle)

  val numSamples = numRadii * numSamplesPerRadius + 1
  val radii = (1 to numRadii).map(_ * stepSize)
  val angles = (0 until numSamplesPerRadius).map(_ * 2 * math.Pi / numSamplesPerRadius)

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
    val scaleFactor = if (normalizeScale) {
      assert(keyPoint.size > 0)
      keyPoint.size / 10.0
    } else 1

    val rotationOffset = if (normalizeRotation) {
      assert(keyPoint.angle != -1)
      keyPoint.angle * 2 * math.Pi / 360
    } else 0

    //    println(rotationOffset)

    samplePattern(scaleFactor, rotationOffset).map(_ + DenseVector(keyPoint.pt.x, keyPoint.pt.y))
  }

  def extractSingle: ExtractorActionSingle = (image, keyPoint) => {
    val blurred = ImageUtil.boxBlur(blurWidth, image)

    val pointOptions = samplePoints(keyPoint).map(point => blurred.getSubPixel(point(0), point(1)))
    if (pointOptions.contains(None)) None
    else {
      val unsorted = pointOptions.flatten.flatMap(interpretColor(color))
      Some(SortDescriptor.fromUnsorted(unsorted))
    }
  }
}
