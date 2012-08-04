import org.scalatest.FunSuite
 
import javax.imageio.ImageIO
import java.io.File

class TestAnything extends FunSuite { 
  test("anything") {
    val leftPath = "/u/echristiansen/Dropbox/head_segmentation/SFSPipeline/data/0.png"
    val leftImage = ImageIO.read(new File(leftPath))
    
    val extractor = new SortExtractorOCL(2)
    val sorts = extractor.extractDense(leftImage)
    println(sorts(0)(0))
    
    //JavaCLTutorial1.example
//    ScalaCLTest.run
    
//    val libraryPath = "/u/echristiansen/Dropbox/head_segmentation/SFSPipeline/src/foreign_bin";
//    System.setProperty("jna.library.path", libraryPath);    
//    
//    val leftPath = "/u/echristiansen/Dropbox/head_segmentation/SFSPipeline/data/0.png"
//    val rightPath = "/u/echristiansen/Dropbox/head_segmentation/SFSPipeline/data/1.png"
//    val box = Array.fill(4)(0)
//    NativeInterface.OpenCV.INSTANCE.detectFace(leftPath, box)
//    //val distance = NativeInterface.OpenCV.INSTANCE.lbphDistance(leftPath, rightPath)
//    println(box.toList)
//    //println(distance)
  }
  
//  test("testSinCos") {
//    import scalacl._
//    import scalacl.impl._
//    import com.nativelibs4java.opencl._
//    import org.bridj.Pointer
//    import org.bridj.Pointer._
//    import scala.math._
//    import javax.imageio.ImageIO
//    import java.io.File
////    import Assert._
//    import scala.math._
//    
//    import scalacl._
//    implicit val context = Context.best(CPU)
//    println(context)
//    val n = 100
//    val f = 0.5f
//    val sinCosOutputs: CLArray[Float] = new CLArray[Float](2 * n)
//    val sinCosCode = customCode("""
//      __kernel void sinCos(__global float2* outputs, float f) {
//       int i = get_global_id(0);
//       float c, s = sincos(i * f, &c);
//       outputs[i] = (float2)(s, c);
//      }
//    """)
//    sinCosCode.execute(
//      args = Array(sinCosOutputs, f),
//      writes = Array(sinCosOutputs),
//      globalSizes = Array(n)
//    )
//    val resCL = sinCosOutputs.toArray
//    
//    val resJava = (0 until n).flatMap(i => {
//      val x = i * f
//      Array(sin(x).toFloat, cos(x).toFloat)
//    }).toArray
//    
//    println(sinCosOutputs.toList)
//    
//    //assertArrayEquals(resJava, resCL, 0.00001f)
//  }  
}
