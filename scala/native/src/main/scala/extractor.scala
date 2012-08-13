package nebula

import java.awt.image._

import com.googlecode.javacv.cpp.opencv_features2d._

trait DescriptorLike[A, B] {
  def values(descriptor: A): IndexedSeq[B]
}

case class MySortDescriptor(val values: IndexedSeq[Int]) {
  assert(values.sorted == (0 until values.size))
}

object DescriptorLike {
  implicit def indexedSeq[B] = new DescriptorLike[IndexedSeq[B], B] {
    override def values(descriptor: IndexedSeq[B]) = descriptor
  }

  implicit def sortDescriptor = new DescriptorLike[MySortDescriptor, Int] {
    override def values(descriptor: MySortDescriptor) = descriptor.values
  }
}

object CompileTest {
  def foo[D](descriptor: D)(implicit descriptorLike: DescriptorLike[D, Int]) {
    val values: IndexedSeq[Int] = descriptorLike.values(descriptor)
    println(values)
  }
}

trait DescriptorTrait[A] {
  val values: IndexedSeq[A]
}

case class Descriptor[A](override val values: IndexedSeq[A]) extends DescriptorTrait[A]

// Hmm, maybe I can use implicits cleverly. See below.
case class Permutation(values: IndexedSeq[Int]) {
  assert(values.sorted == (0 until values.size))
}

object Permutation {
  implicit def permutationToIndexedSeq(permutation: Permutation): IndexedSeq[Int] = 
    permutation.values
}

// TODO: This duplicates |Permutation|.
case class SortDescriptor(override val values: IndexedSeq[Int]) extends DescriptorTrait[Int] {
  assert(values.sorted == (0 until values.size))
}

object SortDescriptor {
  def fromOrdered[A <% Ordered[A]](values: Seq[A]): SortDescriptor = {
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
      numCycles += {if (seen.contains(start)) 0 else 1}
      var current = permutation.values(start)
      while (current != start) {
	seen += current
	current = permutation.values(current)
      }
    }
    numCycles
  }  
}

case class ImagePoint(val x: Int, val y: Int, val z: Int)

case class LBPExtractor() {
  val patchWidth = 3

  val pairs = for (x <- 0 until 3; y <- 0 until 3; if x != 1 || y != 1) yield {
    (ImagePoint(1, 1, 0), ImagePoint(x, y, 0))
  }

  protected def extractUnsafe(image: BufferedImage): Descriptor[Boolean] = {
    val values = for ((left, right) <- pairs) yield {
      assert(left.z == 0 && right.z == 0)
      val leftIntensity = Pixel.getPixel(image, left.x, left.y).gray
      val rightIntensity = Pixel.getPixel(image, right.x, right.y).gray
      leftIntensity < rightIntensity
    }    
    new Descriptor(values)
  }
}

case class BRIEFExtractor(val numPairs: Int, val patchWidth: Int)  {
  def randomPoint: ImagePoint = {
    val x = Global.random.nextInt(patchWidth)
    val y = Global.random.nextInt(patchWidth)
    val z = Global.random.nextInt(3) + 1 // The first channel is alpha.
    ImagePoint(x, y, z)
  }

  val pairs = for (_ <- 0 until numPairs) yield (randomPoint, randomPoint)

  protected def extractUnsafe(image: BufferedImage): Descriptor[Boolean] = {
    val values = for ((left, right) <- pairs) yield {
      val leftIntensity = Pixel.getPixel(image, left.x, left.y)(left.z)
      val rightIntensity = Pixel.getPixel(image, right.x, right.y)(right.z)
      leftIntensity < rightIntensity
    }    
    new Descriptor(values)
  }
}

//------------------------------------------------------------------------------

trait ExtractorMethod extends CorrespondenceMethod {
  // TODO: Allow descriptors that are not SortDescriptor.
  def apply(image: BufferedImage, keyPoint: KeyPoint): Option[SortDescriptor]

  def apply(image: BufferedImage,
	    keyPoints: List[KeyPoint]): List[Option[SortDescriptor]] = {
    keyPoints.map(k => apply(image, k))
  }
}

object ExtractorMethod {
  val instances: List[java.lang.Class[_]] = List(classOf[SortExtractor])
}

case class SortExtractor(val normalizeRotation: Boolean,
			 val normalizeScale: Boolean,
			 val patchWidth: Int,
			 val blurWidth: Int,
			 val color: Boolean) extends ExtractorMethod {
  // TODO
  assert(!normalizeRotation)
  assert(!normalizeScale)

  def apply(image: BufferedImage, keyPoint: KeyPoint): Option[SortDescriptor] = {
    val blurred = ImageProcessing.boxBlur(blurWidth, image)
    val patchOption = ImageProcessing.extractPatch(blurred, patchWidth, keyPoint)
    for (
      patch <- patchOption
    ) yield {
      val pixels = if (color) Pixel.getPixels(patch) else Pixel.getPixelsGray(patch)
      SortDescriptor.fromOrdered(pixels)
    }
  }
}
