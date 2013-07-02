package nebula.graveyard

import java.awt.image._

// abstract class AbstractLBP[A <: DescriptorTrait[_]] {
//   val descriptorExtractor: DescriptorExtractor[A]
//   val patchWidth: Int
//   val gridShape: Tuple2[Int, Int]
//   val imageShape: Tuple2[Int, Int]

//   def histogram(image: BufferedImage): Map[A, Double] = {
//     val dense = descriptorExtractor.extractDense(image).flatten
//     val grouped = dense.groupBy(x => x)
//     grouped.map(x => (x._1, x._2.size.toDouble / dense.size.toDouble))
//   }

//   def chiSquareDistance(leftHistogram: Map[A, Double], rightHistogram: Map[A, Double]): Double = {
//     val activeKeys = leftHistogram.keySet.union(rightHistogram.keySet)
//     val errors = for (key <- activeKeys toList) yield {
//       val leftBin: Double = leftHistogram.getOrElse(key, 0.0)
//       val rightBin: Double = rightHistogram.getOrElse(key, 0.0)
//       math.pow(leftBin - rightBin, 2) / (leftBin + rightBin)
//     }
//     errors.sum
//   }
  
//   def descriptor(image: BufferedImage): List[Map[A, Double]] = {
//     val briefArrayHeight = imageShape._1 - patchWidth + 1
//     val briefArrayWidth = imageShape._2 - patchWidth + 1
//     val cellHeight = briefArrayHeight / gridShape._1
//     val cellWidth = briefArrayWidth / gridShape._2

//     for (yIndex <- 0 until gridShape._1 toList; xIndex <- 0 until gridShape._2 toList) yield {
//       val y = yIndex * cellHeight
//       val height = cellHeight + patchWidth - 1
//       val x = xIndex * cellWidth
//       val width = cellWidth + patchWidth - 1
//       val subimage = image.getSubimage(x, y, width, height)
//       histogram(subimage)
//     }    
//   }

//   def distance(left: BufferedImage, right: BufferedImage): Double = {
//     assert(left.getHeight == imageShape._1 && left.getWidth == imageShape._2)
//     assert(right.getHeight == imageShape._1 && right.getWidth == imageShape._2)
    
//     val leftDescriptor = descriptor(left)
//     val rightDescriptor = descriptor(right)
//     val summands = leftDescriptor.zip(rightDescriptor).map({case (l, r) => chiSquareDistance(l, r)})
//     summands.sum
//   }
// }

// case class SLBP(override val gridShape: Tuple2[Int, Int], override val imageShape: Tuple2[Int, Int]) extends AbstractLBP[Descriptor[Boolean]] {
//   override val patchWidth = 3
//   override val descriptorExtractor = LBPExtractor()
// }

// case class BLBP(val numPairs: Int, override val patchWidth: Int, override val gridShape: Tuple2[Int, Int], override val imageShape: Tuple2[Int, Int]) extends AbstractLBP[Descriptor[Boolean]] {
//   override val descriptorExtractor = BRIEFExtractor(numPairs, patchWidth)
// }

// case class FLBP(override val patchWidth: Int, override val gridShape: Tuple2[Int, Int], override val imageShape: Tuple2[Int, Int]) extends AbstractLBP[SortDescriptor] {
//   override val descriptorExtractor = new GraySortExtractor(patchWidth)
// }

