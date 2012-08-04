package nebula

import java.awt.image._
import java.io.File

case class SortDescriptor(override val values: IndexedSeq[Int]) extends DescriptorTrait[Int]

class SortExtractor(override val patchWidth: Int) extends DescriptorExtractor[SortDescriptor] {
  protected def extractUnsafe(image: BufferedImage) = {
    val pixels: List[Int] = Pixel.getPixels(image)
    SortExtractor.extractFromPixels(pixels)
  }
}

class GraySortExtractor(override val patchWidth: Int) extends DescriptorExtractor[SortDescriptor] {
  protected def extractUnsafe(image: BufferedImage) = {
    val pixels: List[Int] = Pixel.getPixelsGray(image)
    SortExtractor.extractFromPixels(pixels)
  }
}

object SortExtractor {
  def extractFromPixels(pixels: List[Int]): SortDescriptor = {
    val permutation = pixels.zipWithIndex.sortBy(_._1).map(_._2)
    new SortDescriptor(permutation.toIndexedSeq)
  }
}

  


