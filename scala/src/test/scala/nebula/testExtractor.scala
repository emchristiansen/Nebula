package nebula

import org.scalatest.FunSuite
import org.opencv.features2d._
import javax.imageio.ImageIO
import java.io.File
import org.opencv.core.MatOfKeyPoint
import org.opencv.features2d.{ FeatureDetector, KeyPoint }
import nebula._
import org.opencv.core.Mat
import java.awt.Color
import java.awt.image.BufferedImage
import org.apache.commons.math3.linear.Array2DRowRealMatrix
import nebula.util.Homography
import nebula.util.OpenCVUtil
import nebula.util.KeyPointUtil

///////////////////////////////////////////////////////////

class TestExtractor extends FunSuite {
//  test("uniformRank") {
//    val pixels = IndexedSeq(2, 2, 3, 3, 3)
//    val uniformRank = Extractor.uniformRank(pixels)
//    assert(uniformRank === IndexedSeq(1, 1, 3, 3, 3))
//  }

  ignore("BRISK") {
    System.loadLibrary("opencv_java")

    val url = getClass.getResource("/goldfish_girl.jpg")
    val image = ImageIO.read(new File(url.getFile))

    val detector = FeatureDetector.create(FeatureDetector.BRISK)
    val matImage = OpenCVUtil.bufferedImageToMat(image)
    val keyPoints = new MatOfKeyPoint
    detector.detect(matImage, keyPoints)

    val keyPoint = keyPoints.toArray.sortBy(_.response).reverse.head

    val homography =
      Homography(new Array2DRowRealMatrix(Array(
        Array(1.0, 0.0, 0.0),
        Array(0.0, 1.0, 0.0),
        Array(0.0, 0.0, 1.0))))

    val rightKeyPoint = KeyPointUtil.transform(homography)(keyPoint)
  }
  
  test("elucid") {
    val extractor = ELUCIDExtractor(true, true, 4, 2, 2, 5, "Gray")

    val url = getClass.getResource("/goldfish_girl.jpg")
    val image = ImageIO.read(new File(url.getFile))

    assert(extractor.extractSingle(image, new KeyPoint(0, 0, 1, 0, 1, 1, 1)) == None)
    assert(extractor.extractSingle(image, new KeyPoint(5, 5, 1, 0, 1, 1, 1)).isDefined)
  }

  ignore("elucid bikes") {
    System.loadLibrary("opencv_java")    
    
    def drawPointsOnBikes(image: BufferedImage, file: File) {
      val detector = BoundedDetector(OpenCVDetector.BRISK, 60)
      val keyPoints = detector.detect(image)

      val extractor = ELUCIDExtractor(
        true,
        true,
        16,
        0.3,
        4,
        5,
        "Gray")

//      val blurred = ImageUtil.boxBlur(5, image)
//      val graphics = blurred.getGraphics
//      for (keyPoint <- keyPoints) {
//        val samplePoints = extractor.samplePoints(keyPoint)
//        for ((point, index) <- samplePoints.zipWithIndex) {
//          val percentDone = index.toFloat / (samplePoints.size - 1)
//          val color = Color.getHSBColor(percentDone, 1, 1)
//          graphics.setColor(color)
//          graphics.fillOval(point(0).round.toInt - 1, point(1).round.toInt - 1.toInt, 2, 2)
//        }
//      }
//
//      ImageIO.write(blurred, "bmp", file)
    }

    val image1 = {
      val url = getClass.getResource("/oxfordImages/boat/images/img1.bmp")
      ImageIO.read(new File(url.getFile))
    }

    val image2 = {
      val url = getClass.getResource("/oxfordImages/boat/images/img2.bmp")
      ImageIO.read(new File(url.getFile))
    }     
    
    val image3 = {
      val url = getClass.getResource("/oxfordImages/boat/images/img3.bmp")
      ImageIO.read(new File(url.getFile))
    }    
    
    drawPointsOnBikes(image1, new File("/tmp/test_elucid_bikes1.bmp"))
    drawPointsOnBikes(image2, new File("/tmp/test_elucid_bikes2.bmp"))
    drawPointsOnBikes(image3, new File("/tmp/test_elucid_bikes3.bmp"))
  }  
}