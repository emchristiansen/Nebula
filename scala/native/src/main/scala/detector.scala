package nebula

import java.awt.image._

import com.googlecode.javacv.cpp.opencv_contrib._
import com.googlecode.javacv.cpp.opencv_core._
import com.googlecode.javacv.cpp.opencv_features2d._

trait DetectorMethod extends CorrespondenceMethod {
  val maxKeyPoints: Int
  def apply(image: BufferedImage): List[KeyPoint]
}

object DetectorMethod {
  val instances = List(classOf[FASTDetector])
}

case class FASTDetector(val maxKeyPoints: Int) extends DetectorMethod {
  def apply(image: BufferedImage): List[KeyPoint] = {
    val detector = FeatureDetector.create("FAST")
    val matImage = OpenCVUtil.bufferedImageToCvMat(image)
    val keyPoints = new KeyPoint()
    detector.get().detect(matImage, keyPoints, null)
    KeyPointUtil.keyPointsToList(keyPoints)
  }
}
