import org.scalatest.FunSuite
import org.opencv.features2d._
import javax.imageio.ImageIO
import java.io.File
import org.opencv.core.MatOfKeyPoint
import org.opencv.features2d.{FeatureDetector, KeyPoint}
import nebula._
import org.apache.xmlgraphics.image.loader.ImageManager
import org.opencv.core.Mat

class TestAnything extends FunSuite {
  val source = """
  import nebula._

    val experiment1 = CorrespondenceExperiment[FASTDetector, SortDescriptor, SortExtractor, L0Matcher](
      "bikes", 
      2, 
      FASTDetector(100),
      SortExtractor(false, false, 8, 5, true),
      L0Matcher())
  experiment1
"""

  test("anything") {
    System.loadLibrary("opencv_java")    
    
    val url = getClass.getResource("/goldfish_girl.jpg")
    val image = ImageIO.read(new File(url.getFile))    
    
    val detector = FeatureDetector.create(FeatureDetector.BRISK)
    val matImage = OpenCVUtil.bufferedImageToMat(image)
    val keyPoints = new MatOfKeyPoint
    detector.detect(matImage, keyPoints)
    
    
    val keyPoints2 = keyPoints.toArray.sortBy(_.response).reverse.take(10)
    
//    keyPoints2.foreach(println)
    
    val extractor = BRISKDescriptorExtractor.create
    val descriptors = new Mat
    val rawPixels = new Mat
//    extractor.getRawPixels(
//        matImage,
//        new Mat,
//        true,
//        true,
//        true,
//        keyPoints,
//        descriptors,
//        rawPixels)
    
      
    
    //    val x = DenseVector.zeros[Double](2)
    //    println(x)
    ////    val y = new DenseMatrix[Double](5, 4)
    //    val y = DenseMatrix.tabulate(4, 3)((i, j) => "hi")
    //    println(y)
  }
}
