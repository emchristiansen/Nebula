package nebula

import java.awt.image.BufferedImage
import org.opencv.features2d.KeyPoint
import org.opencv.features2d.DescriptorExtractor
import org.opencv.core.Mat
import org.opencv.core.MatOfKeyPoint
import org.opencv.core.CvType

import grizzled.math._

sealed trait Descriptor {
  val thisType: Manifest[_]

  def asType[E: Manifest]: E = {
    assert(thisType <:< implicitly[Manifest[E]])
    this.asInstanceOf[E]
  }

  def values[E: Manifest]: IndexedSeq[E] = {
    def helper[E: Manifest] = {
      val manifest = implicitly[Manifest[E]]
      this match {
        case d: SortDescriptor if implicitly[Manifest[Int]] <:< manifest =>
          d.values.asInstanceOf[IndexedSeq[E]]
        case d: RawDescriptor[_] if d.elementType <:< manifest =>
          d.values.asInstanceOf[IndexedSeq[E]]
        case _ => sys.error("failed match")
      }
    }

    val manifest = implicitly[Manifest[E]]
    // A hack to deal with Doubles.
    if (manifest == implicitly[Manifest[Double]]) {
      try {
        helper[Double].asInstanceOf[IndexedSeq[E]]
      } catch {
        case _ => helper[Int].map(_.toDouble).asInstanceOf[IndexedSeq[E]]
      }
    } else helper[E]
  }
}

case class RawDescriptor[E: Manifest](val values: IndexedSeq[E]) extends Descriptor {
  val elementType = implicitly[Manifest[E]]
  override val thisType = implicitly[Manifest[RawDescriptor[E]]]
}

object RawDescriptor {
  implicit def toIndexedSeq[E](d: RawDescriptor[E]) = d.values
}

trait PermutationLike[A] {
  def invert: A
  def compose(otherPermutation: A): A
  def numCycles: Int
}

object PermutationLike {
  implicit def sortDescriptor(sort: SortDescriptor) = new PermutationLike[SortDescriptor] {
    override def invert = SortDescriptor.invert(sort)
    override def compose(otherPermutation: SortDescriptor) =
      SortDescriptor.compose(sort, otherPermutation)
    override def numCycles = SortDescriptor.numCycles(sort)
  }
}

case class SortDescriptor(
  val values: IndexedSeq[Int]) extends Descriptor {
  assert(values.sorted == (0 until values.size))
  override val thisType = implicitly[Manifest[SortDescriptor]]
}

object SortDescriptor {
  implicit def toIndexedSeq(sort: SortDescriptor) = sort.values

  def fromUnsorted[A <% Ordered[A]](values: Seq[A]): SortDescriptor = {
    val permutation = values.zipWithIndex.sortBy(_._1).map(_._2)
    SortDescriptor(permutation.toIndexedSeq)
  }

  def invert(permutation: SortDescriptor): SortDescriptor = {
    val values = permutation.zipWithIndex.sortBy(_._1).map(_._2)
    SortDescriptor(values)
  }

  def compose(left: SortDescriptor, right: SortDescriptor): SortDescriptor = {
    val values = for (r <- right) yield left(r)
    SortDescriptor(values)
  }

  def numCycles(permutation: SortDescriptor): Int = {
    val seen = collection.mutable.Set[Int]()
    var numCycles = 0
    for (start <- permutation) {
      numCycles += { if (seen.contains(start)) 0 else 1 }
      var current = permutation(start)
      while (current != start) {
        seen += current
        current = permutation(current)
      }
    }
    numCycles
  }
}

case class ImagePoint(val x: Int, val y: Int, val z: Int)

// case class LBPExtractor() {
//   val patchWidth = 3

//   val pairs = for (x <- 0 until 3; y <- 0 until 3; if x != 1 || y != 1) yield {
//     (ImagePoint(1, 1, 0), ImagePoint(x, y, 0))
//   }

//   protected def extractUnsafe(image: BufferedImage): Descriptor[Boolean] = {
//     val values = for ((left, right) <- pairs) yield {
//       assert(left.z == 0 && right.z == 0)
//       val leftIntensity = Pixel.getPixel(image, left.x, left.y).gray
//       val rightIntensity = Pixel.getPixel(image, right.x, right.y).gray
//       leftIntensity < rightIntensity
//     }    
//     new Descriptor(values)
//   }
// }

// case class BRIEFExtractor(val numPairs: Int, val patchWidth: Int)  {
//   def randomPoint: ImagePoint = {
//     val x = Global.random.nextInt(patchWidth)
//     val y = Global.random.nextInt(patchWidth)
//     val z = Global.random.nextInt(3) + 1 // The first channel is alpha.
//     ImagePoint(x, y, z)
//   }

//   val pairs = for (_ <- 0 until numPairs) yield (randomPoint, randomPoint)

//   protected def extractUnsafe(image: BufferedImage): Descriptor[Boolean] = {
//     val values = for ((left, right) <- pairs) yield {
//       val leftIntensity = Pixel.getPixel(image, left.x, left.y)(left.z)
//       val rightIntensity = Pixel.getPixel(image, right.x, right.y)(right.z)
//       leftIntensity < rightIntensity
//     }    
//     new Descriptor(values)
//   }
// }

//------------------------------------------------------------------------------

sealed trait Extractor {
  import ExtractorImpl._

  def extract: ExtractorAction
}

object ExtractorParameterized {
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
}

///////////////////////////////////////////////////////////

object ExtractorImpl {
  type ExtractorActionSingle = (BufferedImage, KeyPoint) => Option[Descriptor]

  type ExtractorAction = (BufferedImage, Seq[KeyPoint]) => Seq[Option[Descriptor]]

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

    val blurred = ImageProcessing.boxBlur(blurWidth, image)
    val patchOption = ImageProcessing.extractPatch(blurred, patchWidth, keyPoint)
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

case class RawExtractor(
  val normalizeRotation: Boolean,
  val normalizeScale: Boolean,
  val patchWidth: Int,
  val blurWidth: Int,
  val color: String) extends Extractor {
  import ExtractorImpl._

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
  val normalizeRotation: Boolean,
  val normalizeScale: Boolean,
  val patchWidth: Int,
  val blurWidth: Int,
  val color: String) extends Extractor {
  import ExtractorImpl._

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
  val normalizeRotation: Boolean,
  val normalizeScale: Boolean,
  val patchWidth: Int,
  val blurWidth: Int,
  val color: String) extends Extractor {
  import ExtractorImpl._

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
  val normalizeRotation: Boolean,
  val normalizeScale: Boolean,
  val patchWidth: Int,
  val blurWidth: Int,
  val color: String) extends Extractor {
  import ExtractorImpl._

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
  val normalizeRotation: Boolean,
  val normalizeScale: Boolean,
  val patchWidth: Int,
  val blurWidth: Int,
  val color: String) extends Extractor {
  import ExtractorImpl._

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
  val normalizeRotation: Boolean,
  val normalizeScale: Boolean,
  val patchWidth: Int,
  val blurWidth: Int,
  val color: String) extends Extractor {
  import ExtractorImpl._

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
        ExtractorParameterized.uniformRank(unsorted)
      }
    }
}

case class BRISKExtractor(
  val normalizeRotation: Boolean,
  val normalizeScale: Boolean) extends Extractor {
  import ExtractorImpl._

  def extract = applySeveral(extractSingle)

  // Doing this one by one is super slow.
  // TODO: Make the native OpenCV api less awful.
  def extractSingle: ExtractorActionSingle =
    booleanExtractorFromEnum(DescriptorExtractor.BRISK)
}

case class BRISKRawExtractor(
  val normalizeRotation: Boolean,
  val normalizeScale: Boolean) extends Extractor {
  import ExtractorImpl._

  def extract = applySeveral(extractSingle)

  // Doing this one by one is super slow.
  // TODO: Make the native OpenCV api less awful.
  def extractSingle: ExtractorActionSingle = //sys.error("TODO")
    intExtractorFromEnum(DescriptorExtractor.BRISKLUCID)
}

case class BRISKOrderExtractor(
  val normalizeRotation: Boolean,
  val normalizeScale: Boolean) extends Extractor {
  import ExtractorImpl._

  def extract = applySeveral(extractSingle)

  // Doing this one by one is super slow.
  // TODO: Make the native OpenCV api less awful.
  def extractSingle: ExtractorActionSingle = (image, keyPoint) => {
    //    sys.error("TODO")
    for (descriptor <- intExtractorFromEnum(DescriptorExtractor.BRISKLUCID)(image, keyPoint)) yield SortDescriptor.fromUnsorted(descriptor.values[Int])
  }
}

case class BRISKRankExtractor(
  val normalizeRotation: Boolean,
  val normalizeScale: Boolean) extends Extractor {
  import ExtractorImpl._

  def extract = applySeveral(extractSingle)

  // Doing this one by one is super slow.
  // TODO: Make the native OpenCV api less awful.
  def extractSingle: ExtractorActionSingle = (image, keyPoint) => {
    //    sys.error("TODO")
    for (descriptor <- intExtractorFromEnum(DescriptorExtractor.BRISKLUCID)(image, keyPoint)) yield SortDescriptor.fromUnsorted(SortDescriptor.fromUnsorted(descriptor.values[Int]))
  }
}

case class FREAKExtractor(
  val normalizeRotation: Boolean,
  val normalizeScale: Boolean) extends Extractor {
  import ExtractorImpl._

  def extract = applySeveral(extractSingle)

  // Doing this one by one is super slow.
  // TODO: Make the native OpenCV api less awful.
  def extractSingle: ExtractorActionSingle =
    booleanExtractorFromEnum(DescriptorExtractor.FREAK)
}

case class BRIEFExtractor(
  val normalizeRotation: Boolean,
  val normalizeScale: Boolean) extends Extractor {
  import ExtractorImpl._

  def extract = applySeveral(extractSingle)

  // Doing this one by one is super slow.
  // TODO: Make the native OpenCV api less awful.
  def extractSingle: ExtractorActionSingle =
    booleanExtractorFromEnum(DescriptorExtractor.BRIEF)
}

case class ORBExtractor(
  val normalizeRotation: Boolean,
  val normalizeScale: Boolean) extends Extractor {
  import ExtractorImpl._

  def extract = applySeveral(extractSingle)

  // Doing this one by one is super slow.
  // TODO: Make the native OpenCV api less awful.
  def extractSingle: ExtractorActionSingle =
    booleanExtractorFromEnum(DescriptorExtractor.ORB)
}

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import RichImage._

case class ELUCIDExtractor(
  val normalizeRotation: Boolean,
  val normalizeScale: Boolean,
  val numSamplesPerRadius: Int,
  val stepSize: Double,
  val numRadii: Int,
  val blurWidth: Int,
  val color: String) extends Extractor {
  import ExtractorImpl._

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
    val blurred = ImageProcessing.boxBlur(blurWidth, image)

    val pointOptions = samplePoints(keyPoint).map(point => blurred.getSubPixel(point(0), point(1)))
    if (pointOptions.contains(None)) None
    else {
      val unsorted = pointOptions.flatten.flatMap(interpretColor(color))
      Some(SortDescriptor.fromUnsorted(unsorted))
    }
  }
}
