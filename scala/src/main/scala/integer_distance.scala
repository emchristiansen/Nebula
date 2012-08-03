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

class SortExtractorOCL(override val patchWidth: Int) extends SortExtractor(patchWidth) {
  override def extractDense(image: BufferedImage): IndexedSeq[IndexedSeq[SortDescriptor]] = {
//    val cachePath = "/tmp/debug/extractDense_%s".format(Pixel.getPixels(image).hashCode)
//    if (new File(cachePath).exists) {
//      println("loading cached descriptors")
//      val descriptors = IO.ReadObjectFromFilename[IndexedSeq[IndexedSeq[SortDescriptor]]](cachePath)
//      return descriptors
//    }
    
    import com.nativelibs4java.opencl._
    import com.nativelibs4java.opencl.CLMem.Usage
    import com.nativelibs4java.opencl.util._
    import com.nativelibs4java.util._
    import org.bridj.Pointer
    import org.bridj.Pointer._
    
    import OpenCLUtil._
    
    val source = {
      // TODO: fix source
      val raw = io.Source.fromFile("/u/echristiansen/Dropbox/head_segmentation/SFSPipeline/src/opencl/extract_sorts.cl").mkString
      val substitutions = Map(
          "height" -> image.getHeight.toString, 
          "width" -> image.getWidth.toString,
          "depth" -> 3.toString,
          "patchWidth" -> patchWidth.toString)
      Util.bashStyleReplace(substitutions, raw)
    }
//    println(source)
    
    val sortHeight = image.getHeight - patchWidth + 1
    val sortWidth = image.getWidth - patchWidth + 1
    val descriptorSize = patchWidth * patchWidth * 3
    
    val queue = context.createDefaultQueue()
    
    val imageCL = context.createImage2D(Usage.Input, image, false)
    val sortsSize = sortHeight * sortWidth * descriptorSize
    val sortsBuffer = context.createIntBuffer(Usage.Output, sortsSize)
    
    val program = context.createProgram(source)
    val kernel = program.createKernel("extractSorts")
    kernel.setArgs(imageCL, sortsBuffer)
    val clEvent = kernel.enqueueNDRange(queue, Array(sortHeight, sortWidth))
                 
    val sortsCL = sortsBuffer.read(queue, clEvent)
    val sorts = clToArray(sortsSize, sortsCL)
    
    val sortDescriptors = sorts.grouped(descriptorSize).map(_.toIndexedSeq).map(SortDescriptor)
    val ordered = sortDescriptors.grouped(sortWidth).map(_.toIndexedSeq).toIndexedSeq
    
//    IO.WriteObjectToFilename(ordered, cachePath)
    
    ordered
  }  
  
//  override def extractDense(image: BufferedImage): IndexedSeq[IndexedSeq[SortDescriptor]] = {
//    import scalacl._
//    import scalacl.impl._
//    import com.nativelibs4java.opencl._
//    import org.bridj.Pointer
//    import org.bridj.Pointer._
//    import scala.math._
//    import javax.imageio.ImageIO
//    import java.io.File  
//  
//    import OpenCLUtil._
//    
//    val source = {
//      // TODO: fix source
//      val raw = io.Source.fromFile("/u/echristiansen/Dropbox/head_segmentation/SFSPipeline/src/opencl/extract_sorts.cl").mkString
//      val substitutions = Map(
//          "height" -> image.getHeight.toString, 
//          "width" -> image.getWidth.toString, 
//          "depth" -> 3.toString, 
//          "patchWidth" -> patchWidth.toString)
//      Util.bashStyleReplace(substitutions, raw)
//    }
//    
//    val sortHeight = image.getHeight - patchWidth + 1
//    val sortWidth = image.getWidth - patchWidth + 1
//    val descriptorSize = patchWidth * patchWidth * 3
//    
//    val imageArray = {
//      val pixels = Pixel.getPixels(image)
//      assert(pixels.min >= 0 && pixels.max < 256)
//      pixels.toCLArray 
//    }
//    val sortArray = new CLArray[Int](sortHeight * sortWidth * descriptorSize)
//    val code = customCode(source)
//    code.execute(args = Array(imageArray, sortArray), 
//                 writes = Array(sortArray),
//                 globalSizes = Array(sortHeight, sortWidth),
//                 kernelName = "extractSorts")                 
//                 
//    val sortDescriptors = sortArray.toArray.grouped(descriptorSize).map(_.toIndexedSeq).map(SortDescriptor)
//    val ordered = sortDescriptors.grouped(sortWidth).map(_.toIndexedSeq).toIndexedSeq
//    ordered
//  }
}


