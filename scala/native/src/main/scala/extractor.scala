package nebula

import java.awt.image._

import com.googlecode.javacv.cpp.opencv_features2d._

// sealed trait DescriptorTraitAbstract

trait DescriptorTrait[A] {
  val values: IndexedSeq[A]
}

case class Descriptor[A](override val values: IndexedSeq[A]) extends DescriptorTrait[A]

case class SortDescriptor(override val values: IndexedSeq[Int]) extends DescriptorTrait[Int] {
  assert(values.sorted == (0 until values.size))
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
  def apply(image: BufferedImage, keyPoint: KeyPoint): Option[SortDescriptor] = {
    val blurred = ImageProcessing.boxBlur(blurWidth, image)
    val patchOption = ImageProcessing.extractPatch(blurred, patchWidth, keyPoint)
    patchOption match {
      case Some(patch) => {
	val pixels = Pixel.getPixels(patch)
	val permutation = pixels.zipWithIndex.sortBy(_._1).map(_._2)
	Some(SortDescriptor(permutation.toIndexedSeq))
      }
      case None => None
    }
  }
}
