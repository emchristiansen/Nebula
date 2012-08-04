package nebula

import java.awt.image._

// This class exists because it's bad to subclass a case class, and I want
// the descriptors to be case classes.
abstract class DescriptorTrait[A] {
  val values: IndexedSeq[A]
}

case class Descriptor[A](override val values: IndexedSeq[A]) extends DescriptorTrait[A]

object DescriptorDistance {
  def l0[A](left: DescriptorTrait[A], right: DescriptorTrait[A]): Int = {
    left.values.zip(right.values).count({case (l, r) => l != r})
  }

  def l1(left: DescriptorTrait[Int], right: DescriptorTrait[Int]): Int = {
    left.values.zip(right.values).map({case (l, r) => (l - r).abs}).sum
  }

  def kendallTau(left: SortDescriptor, right: SortDescriptor): Int = {
    val size = left.values.size
    assert(size == right.values.size)
    val errors = for (i <- 0 until size; j <- i + 1 until size) yield {
      if (left.values(i) < left.values(j) == right.values(i) < right.values(j)) 0
      else 1
    }
    errors.sum
  }
}

abstract class DescriptorExtractor[A <: DescriptorTrait[_]] {
  val patchWidth: Int

  protected def extractUnsafe(image: BufferedImage): A

  def extract(image: BufferedImage): A = {
    assert(image.getHeight == patchWidth && image.getWidth == patchWidth)
    extractUnsafe(image)
  }

  def extractAtPoint(y: Int, x: Int, image: BufferedImage): A = {
    extract(image.getSubimage(x, y, patchWidth, patchWidth))
  }

  def extractDense(image: BufferedImage): IndexedSeq[IndexedSeq[A]] = {
    val descriptors =
      for (y <- 0 until (image.getHeight - patchWidth + 1) par) yield {
	for (x <- 0 until (image.getWidth - patchWidth + 1)) yield {
	  extractAtPoint(y, x, image)
	}
      }
    descriptors.toIndexedSeq
  }
}

case class ImagePoint(val x: Int, val y: Int, val z: Int)

case class LBPExtractor() extends DescriptorExtractor[Descriptor[Boolean]] {
  override val patchWidth = 3

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

case class BRIEFExtractor(val numPairs: Int, override val patchWidth: Int) extends DescriptorExtractor[Descriptor[Boolean]] {
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

abstract class AbstractLBP[A <: DescriptorTrait[_]] {
  val descriptorExtractor: DescriptorExtractor[A]
  val patchWidth: Int
  val gridShape: Tuple2[Int, Int]
  val imageShape: Tuple2[Int, Int]

  def histogram(image: BufferedImage): Map[A, Double] = {
    val dense = descriptorExtractor.extractDense(image).flatten
    val grouped = dense.groupBy(x => x)
    grouped.map(x => (x._1, x._2.size.toDouble / dense.size.toDouble))
  }

  def chiSquareDistance(leftHistogram: Map[A, Double], rightHistogram: Map[A, Double]): Double = {
    val activeKeys = leftHistogram.keySet.union(rightHistogram.keySet)
    val errors = for (key <- activeKeys toList) yield {
      val leftBin: Double = leftHistogram.getOrElse(key, 0.0)
      val rightBin: Double = rightHistogram.getOrElse(key, 0.0)
      math.pow(leftBin - rightBin, 2) / (leftBin + rightBin)
    }
    errors.sum
  }
  
  def descriptor(image: BufferedImage): List[Map[A, Double]] = {
    val briefArrayHeight = imageShape._1 - patchWidth + 1
    val briefArrayWidth = imageShape._2 - patchWidth + 1
    val cellHeight = briefArrayHeight / gridShape._1
    val cellWidth = briefArrayWidth / gridShape._2

    for (yIndex <- 0 until gridShape._1 toList; xIndex <- 0 until gridShape._2 toList) yield {
      val y = yIndex * cellHeight
      val height = cellHeight + patchWidth - 1
      val x = xIndex * cellWidth
      val width = cellWidth + patchWidth - 1
      val subimage = image.getSubimage(x, y, width, height)
      histogram(subimage)
    }    
  }

  def distance(left: BufferedImage, right: BufferedImage): Double = {
    assert(left.getHeight == imageShape._1 && left.getWidth == imageShape._2)
    assert(right.getHeight == imageShape._1 && right.getWidth == imageShape._2)
    
    val leftDescriptor = descriptor(left)
    val rightDescriptor = descriptor(right)
    val summands = leftDescriptor.zip(rightDescriptor).map({case (l, r) => chiSquareDistance(l, r)})
    summands.sum
  }
}

case class SLBP(override val gridShape: Tuple2[Int, Int], override val imageShape: Tuple2[Int, Int]) extends AbstractLBP[Descriptor[Boolean]] {
  override val patchWidth = 3
  override val descriptorExtractor = LBPExtractor()
}

case class BLBP(val numPairs: Int, override val patchWidth: Int, override val gridShape: Tuple2[Int, Int], override val imageShape: Tuple2[Int, Int]) extends AbstractLBP[Descriptor[Boolean]] {
  override val descriptorExtractor = BRIEFExtractor(numPairs, patchWidth)
}

case class FLBP(override val patchWidth: Int, override val gridShape: Tuple2[Int, Int], override val imageShape: Tuple2[Int, Int]) extends AbstractLBP[SortDescriptor] {
  override val descriptorExtractor = new GraySortExtractor(patchWidth)
}

