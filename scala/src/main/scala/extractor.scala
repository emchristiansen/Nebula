package nebula

import java.awt.image.BufferedImage
import org.opencv.features2d.KeyPoint
import org.opencv.features2d.DescriptorExtractor
import org.opencv.core.Mat
import org.opencv.core.MatOfKeyPoint
import org.opencv.core.CvType

// TODO: Given |D|, |E| can be determined statically. Figure
// out how to reduce this to one parameter.
trait DescriptorLike[D, E] {
  // TODO: Make this interface consistent: It should have an apply
  // function which returns a function which returns the values.
  def values(descriptor: D): IndexedSeq[E]
}

object DescriptorLike {
  implicit def raw[E] = new DescriptorLike[RawDescriptor[E], E] {
    override def values(descriptor: RawDescriptor[E]) = descriptor.values
  }

  implicit def sort = new DescriptorLike[SortDescriptor, Int] {
    override def values(descriptor: SortDescriptor) = descriptor.values
  }
}

sealed trait Descriptor
case class RawDescriptor[A](val values: IndexedSeq[A]) extends Descriptor

trait PermutationLike[A] {
  def invert(permutation: A): A
  def compose(leftPermutation: A, rightPermutation: A): A
  def numCycles(permutation: A): Int
}

object PermutationLike {
  implicit def sortDescriptor = new PermutationLike[SortDescriptor] {
    override def invert(permutation: SortDescriptor) =
      SortDescriptor.invert(permutation)
    override def compose(
      leftPermutation: SortDescriptor,
      rightPermutation: SortDescriptor) =
      SortDescriptor.compose(leftPermutation, rightPermutation)
    override def numCycles(permutation: SortDescriptor) =
      SortDescriptor.numCycles(permutation)
  }
}

case class SortDescriptor(val values: IndexedSeq[Int]) extends Descriptor {
  assert(values.sorted == (0 until values.size))
}

object SortDescriptor {
  def fromUnsorted[A <% Ordered[A]](values: Seq[A]): SortDescriptor = {
    val permutation = values.zipWithIndex.sortBy(_._1).map(_._2)
    SortDescriptor(permutation.toIndexedSeq)
  }

  // TODO: This manual boxing and unboxing is dumb.
  def invert(permutation: SortDescriptor): SortDescriptor = {
    val values = permutation.values.zipWithIndex.sortBy(_._1).map(_._2)
    SortDescriptor(values)
  }

  def compose(left: SortDescriptor, right: SortDescriptor): SortDescriptor = {
    val values = for (r <- right.values) yield left.values(r)
    SortDescriptor(values)
  }

  def numCycles(permutation: SortDescriptor): Int = {
    val seen = collection.mutable.Set[Int]()
    var numCycles = 0
    for (start <- permutation.values) {
      numCycles += { if (seen.contains(start)) 0 else 1 }
      var current = permutation.values(start)
      while (current != start) {
        seen += current
        current = permutation.values(current)
      }
    }
    numCycles
  }

  // implicit def indexedSeq(sortDescriptor: SortDescriptor): IndexedSeq[Int] =
  //   sortDescriptor.values
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

trait ExtractorLike[E, D] {
  import ExtractorImpl._

  def apply(extractor: E): ExtractorAction[D]
}

object ExtractorLike {
  val instances: Seq[Class[_]] = Seq(classOf[RawExtractor], classOf[SortExtractor], classOf[BRISKExtractor], classOf[FREAKExtractor], classOf[ELUCIDExtractor])

  implicit def raw = new ExtractorLike[RawExtractor, RawDescriptor[Int]] {
    override def apply(extractor: RawExtractor) =
      ExtractorImpl.applySeveral(extractor.extractSingle)
  }

  implicit def sort = new ExtractorLike[SortExtractor, SortDescriptor] {
    override def apply(extractor: SortExtractor) =
      ExtractorImpl.applySeveral(extractor.extractSingle)
  }

  implicit def brisk = new ExtractorLike[BRISKExtractor, RawDescriptor[Boolean]] {
    //    override def apply(extractor: BRISKExtractor) = extractor.extract
    override def apply(extractor: BRISKExtractor) =
      ExtractorImpl.applySeveral(extractor.extractSingle)
  }

  implicit def freak = new ExtractorLike[FREAKExtractor, RawDescriptor[Boolean]] {
    override def apply(extractor: FREAKExtractor) =
      ExtractorImpl.applySeveral(extractor.extractSingle)
  }
  
  implicit def elucid = new ExtractorLike[ELUCIDExtractor, SortDescriptor] {
    override def apply(extractor: ELUCIDExtractor) =
      ExtractorImpl.applySeveral(extractor.extractSingle)
  }  
}

object ExtractorImpl {
  type ExtractorActionSingle[D] = (BufferedImage, KeyPoint) => Option[D]

  type ExtractorAction[D] = (BufferedImage, Seq[KeyPoint]) => Seq[Option[D]]

  def applySeveral[D](extractSingle: ExtractorActionSingle[D]): ExtractorAction[D] =
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

  def booleanExtractorFromEnum(enum: Int): ExtractorActionSingle[RawDescriptor[Boolean]] =
    (image: BufferedImage, keyPoint: KeyPoint) => {
      val extractor = DescriptorExtractor.create(enum)
      val imageMat = OpenCVUtil.bufferedImageToMat(image)
      // This will get overwritten, it just matters that |compute| gets
      // a valid |CvMat|.
      val descriptor = new Mat
      extractor.compute(imageMat, new MatOfKeyPoint(keyPoint), descriptor)

      if (descriptor.rows == 0 || descriptor.cols == 0) None
      else {
        assert(descriptor.rows == 1)
        assert(descriptor.cols > 0)
        assert(descriptor.`type` == CvType.CV_8UC1)

        val bits = {
          val ints = for (c <- 0 until descriptor.cols) yield {
            val doubles = descriptor.get(0, c)
            assert(doubles.size == 1)
            doubles.head.toInt
          }

          ints.flatMap(Util.numToBits(8))
        }

        Some(RawDescriptor(bits))
      }
    }
}

sealed trait Extractor

case class RawExtractor(
  val normalizeRotation: Boolean,
  val normalizeScale: Boolean,
  val patchWidth: Int,
  val blurWidth: Int,
  val color: String) extends Extractor {
  import ExtractorImpl._

  // TODO: It's dumb I have to pass these parameters explicitly.
  def extractSingle: ExtractorActionSingle[RawDescriptor[Int]] = rawPixels(
    normalizeRotation,
    normalizeScale,
    patchWidth,
    blurWidth,
    color)
}

case class SortExtractor(
  val normalizeRotation: Boolean,
  val normalizeScale: Boolean,
  val patchWidth: Int,
  val blurWidth: Int,
  val color: String) extends Extractor {
  import ExtractorImpl._

  def extractSingle: ExtractorActionSingle[SortDescriptor] =
    (image: BufferedImage, keyPoint: KeyPoint) => {
      val unsortedOption = rawPixels(
        normalizeRotation,
        normalizeScale,
        patchWidth,
        blurWidth,
        color)(image, keyPoint)
      for (unsorted <- unsortedOption) yield {
        val descriptorLike = implicitly[DescriptorLike[RawDescriptor[Int], Int]]
        SortDescriptor.fromUnsorted(descriptorLike.values(unsorted))
      }
    }
}

case class BRISKExtractor(
  val normalizeRotation: Boolean,
  val normalizeScale: Boolean) extends Extractor {
  import ExtractorImpl._

  // Doing this one by one is super slow.
  // TODO: Make the native OpenCV api less awful.
  def extractSingle: ExtractorActionSingle[RawDescriptor[Boolean]] =
    booleanExtractorFromEnum(DescriptorExtractor.BRISK)
}

case class FREAKExtractor(
  val normalizeRotation: Boolean,
  val normalizeScale: Boolean) extends Extractor {
  import ExtractorImpl._

  // Doing this one by one is super slow.
  // TODO: Make the native OpenCV api less awful.
  def extractSingle: ExtractorActionSingle[RawDescriptor[Boolean]] =
    booleanExtractorFromEnum(DescriptorExtractor.FREAK)
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
  
  def extractSingle: ExtractorActionSingle[SortDescriptor] = (image, keyPoint) => {
    val blurred = ImageProcessing.boxBlur(blurWidth, image)    
    
    val pointOptions = samplePoints(keyPoint).map(point => blurred.getSubPixel(point(0), point(1)))
    if (pointOptions.contains(None)) None
    else {
      val unsorted = pointOptions.flatten.flatMap(interpretColor(color))
      Some(SortDescriptor.fromUnsorted(unsorted))
    }
  }
}
