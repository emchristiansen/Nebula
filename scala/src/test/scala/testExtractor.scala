import org.scalatest.FunSuite
import org.opencv.features2d._
import javax.imageio.ImageIO
import java.io.File
import org.opencv.core.MatOfKeyPoint
import org.opencv.features2d.{ FeatureDetector, KeyPoint }
import nebula._
import org.apache.xmlgraphics.image.loader.ImageManager
import org.opencv.core.Mat
import java.awt.Color
import java.awt.image.BufferedImage
import org.apache.commons.math3.linear.Array2DRowRealMatrix
import nebula.util.Homography
import nebula.util.OpenCVUtil
import nebula.util.KeyPointUtil

class TestExtractor extends FunSuite {
  test("uniformRank") {
    val pixels = IndexedSeq(2, 2, 3, 3, 3)
    val uniformRank = Extractor.uniformRank(pixels)
    assert(uniformRank === IndexedSeq(1, 1, 3, 3, 3))
  }

  test("BRISK") {
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

    val extractor = BRISKExtractor(BRISKExtractorType.Raw, true, true)
    val leftDescriptor = extractor.extractSingle(image, keyPoint)
    val rightDescriptor = extractor.extractSingle(image, rightKeyPoint)

    assert(leftDescriptor === rightDescriptor)
  }
}