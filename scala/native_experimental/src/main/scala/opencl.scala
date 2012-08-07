package nebula.native.experimental

import nebula._
import nebula.native._
import nebula.experimental._

import java.awt.image._

// class SortExtractorOCL(override val patchWidth: Int) extends SortExtractor(patchWidth) {
//   override def extractDense(image: BufferedImage): IndexedSeq[IndexedSeq[SortDescriptor]] = {
// //    val cachePath = "/tmp/debug/extractDense_%s".format(Pixel.getPixels(image).hashCode)
// //    if (new File(cachePath).exists) {
// //      println("loading cached descriptors")
// //      val descriptors = IO.ReadObjectFromFilename[IndexedSeq[IndexedSeq[SortDescriptor]]](cachePath)
// //      return descriptors
// //    }
    
//     import com.nativelibs4java.opencl._
//     import com.nativelibs4java.opencl.CLMem.Usage
//     import com.nativelibs4java.opencl.util._
//     import com.nativelibs4java.util._
//     import org.bridj.Pointer
//     import org.bridj.Pointer._
    
//     import OpenCLUtil._
    
//     val source = {
//       // TODO: fix source
//       val raw = io.Source.fromFile("/u/echristiansen/Dropbox/head_segmentation/SFSPipeline/src/opencl/extract_sorts.cl").mkString
//       val substitutions = Map(
//           "height" -> image.getHeight.toString, 
//           "width" -> image.getWidth.toString,
//           "depth" -> 3.toString,
//           "patchWidth" -> patchWidth.toString)
//       Util.bashStyleReplace(substitutions, raw)
//     }
// //    println(source)
    
//     val sortHeight = image.getHeight - patchWidth + 1
//     val sortWidth = image.getWidth - patchWidth + 1
//     val descriptorSize = patchWidth * patchWidth * 3
    
//     val queue = context.createDefaultQueue()
    
//     val imageCL = context.createImage2D(Usage.Input, image, false)
//     val sortsSize = sortHeight * sortWidth * descriptorSize
//     val sortsBuffer = context.createIntBuffer(Usage.Output, sortsSize)
    
//     val program = context.createProgram(source)
//     val kernel = program.createKernel("extractSorts")
//     kernel.setArgs(imageCL, sortsBuffer)
//     val clEvent = kernel.enqueueNDRange(queue, Array(sortHeight, sortWidth))
                 
//     val sortsCL = sortsBuffer.read(queue, clEvent)
//     val sorts = clToArray(sortsSize, sortsCL)
    
//     val sortDescriptors = sorts.grouped(descriptorSize).map(_.toIndexedSeq).map(SortDescriptor)
//     val ordered = sortDescriptors.grouped(sortWidth).map(_.toIndexedSeq).toIndexedSeq
    
// //    IO.WriteObjectToFilename(ordered, cachePath)
    
//     ordered
//   }

// //  override def extractDense(image: BufferedImage): IndexedSeq[IndexedSeq[SortDescriptor]] = {
// //    import scalacl._
// //    import scalacl.impl._
// //    import com.nativelibs4java.opencl._
// //    import org.bridj.Pointer
// //    import org.bridj.Pointer._
// //    import scala.math._
// //    import javax.imageio.ImageIO
// //    import java.io.File  
// //  
// //    import OpenCLUtil._
// //    
// //    val source = {
// //      // TODO: fix source
// //      val raw = io.Source.fromFile("/u/echristiansen/Dropbox/head_segmentation/SFSPipeline/src/opencl/extract_sorts.cl").mkString
// //      val substitutions = Map(
// //          "height" -> image.getHeight.toString, 
// //          "width" -> image.getWidth.toString, 
// //          "depth" -> 3.toString, 
// //          "patchWidth" -> patchWidth.toString)
// //      Util.bashStyleReplace(substitutions, raw)
// //    }
// //    
// //    val sortHeight = image.getHeight - patchWidth + 1
// //    val sortWidth = image.getWidth - patchWidth + 1
// //    val descriptorSize = patchWidth * patchWidth * 3
// //    
// //    val imageArray = {
// //      val pixels = Pixel.getPixels(image)
// //      assert(pixels.min >= 0 && pixels.max < 256)
// //      pixels.toCLArray 
// //    }
// //    val sortArray = new CLArray[Int](sortHeight * sortWidth * descriptorSize)
// //    val code = customCode(source)
// //    code.execute(args = Array(imageArray, sortArray), 
// //                 writes = Array(sortArray),
// //                 globalSizes = Array(sortHeight, sortWidth),
// //                 kernelName = "extractSorts")                 
// //                 
// //    val sortDescriptors = sortArray.toArray.grouped(descriptorSize).map(_.toIndexedSeq).map(SortDescriptor)
// //    val ordered = sortDescriptors.grouped(sortWidth).map(_.toIndexedSeq).toIndexedSeq
// //    ordered
// //  }
// }  


// object LUM {
//   def l0LUMOCL(patchWidth: Int, radius: Int): LM[SortDescriptor] = {
//     val extractor = new SortExtractorOCL(patchWidth)
//     val distance = DescriptorDistance.l0OCL _
//     val matchScorer = Match.unconstrainedMatchScore[Tuple2[Int, Int]] _
//     LM(extractor, distance, radius, matchScorer)
//   }
// }

// object DescriptorDistance {
//   def l0OCL(indices: IndexedSeq[Tuple2[Int, Int]], 
//             left: IndexedSeq[SortDescriptor], 
//             right: IndexedSeq[SortDescriptor]): IndexedSeq[Int] = {
//     val numComparisons = indices.size
//     require(numComparisons > 0)
    
//     val descriptorDepth = left(0).values.size
//     require(descriptorDepth > 0)
//     require(left.map(_.values.size == descriptorDepth).reduce(_ && _))
//     require(right.map(_.values.size == descriptorDepth).reduce(_ && _))  
    
//     val (leftIndices, rightIndices) = indices.unzip
//     require(leftIndices.min >= 0)
//     require(leftIndices.max < left.size)
//     require(rightIndices.min >= 0)
//     require(rightIndices.max < right.size)
      
//     import com.nativelibs4java.opencl._
//     import com.nativelibs4java.opencl.CLMem.Usage
//     import com.nativelibs4java.opencl.util._
//     import com.nativelibs4java.util._
//     import org.bridj.Pointer
//     import org.bridj.Pointer._
    
//     import OpenCLUtil._
    
//     val context = JavaCL.createBestContext
//     val queue = context.createDefaultQueue()
    
//     val leftIndicesCL = arrayToCL(leftIndices.toArray) 
//     //Util.assertContentsEqual(clToArray(leftIndices.size, leftIndicesCL), leftIndices)
//     val rightIndicesCL = arrayToCL(rightIndices.toArray)
//     //Util.assertContentsEqual(clToArray(rightIndices.size, rightIndicesCL), rightIndices)
      
//     val leftFlat = left.map(_.values).flatten.toArray 
//     val leftCL = arrayToCL(leftFlat)
//     //Util.assertContentsEqual(clToArray(leftFlat.size, leftCL), leftFlat)
//     val rightFlat = right.map(_.values).flatten.toArray
//     val rightCL = arrayToCL(rightFlat)
//     //Util.assertContentsEqual(clToArray(rightFlat.size, rightCL), rightFlat)
    
//     val source = {
//       // TODO: fix source
//       val raw = io.Source.fromFile("/u/echristiansen/Dropbox/head_segmentation/SFSPipeline/src/opencl/l0_distance.cl").mkString
//       val substitutions = Map(
//           "numComparisons" -> numComparisons.toString, 
//           "descriptorDepth" -> descriptorDepth.toString) 
//       Util.bashStyleReplace(substitutions, raw)
//     }
    
//     val leftIndicesBuffer = context.createBuffer(Usage.Input, leftIndicesCL)
//     val rightIndicesBuffer = context.createBuffer(Usage.Input, rightIndicesCL)
//     val leftBuffer = context.createBuffer(Usage.Input, leftCL)
//     val rightBuffer = context.createBuffer(Usage.Input, rightCL)
//     val distancesBuffer = context.createIntBuffer(Usage.Output, numComparisons)

// //    println(source)
    
//     val program = context.createProgram(source)
//     val kernel = program.createKernel("l0Distances")
//     kernel.setArgs(leftIndicesBuffer, rightIndicesBuffer, leftBuffer, rightBuffer, distancesBuffer)
//     val clEvent = kernel.enqueueNDRange(queue, Array(numComparisons))
    
//     val distancesCL = distancesBuffer.read(queue, clEvent)
    
//     val distances = clToArray(numComparisons, distancesCL).toIndexedSeq
//     assert(distances.map(_ >= 0).reduce(_ && _))
//     distances
//   }  
// }
