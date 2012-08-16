package nebula

import java.awt.image._

import com.googlecode.javacv.cpp.opencv_contrib._
import com.googlecode.javacv.cpp.opencv_core._
import com.googlecode.javacv.cpp.opencv_features2d._

trait DetectorLike[T] {
  import DetectorImpl._

  def apply(detector: T): DetectorAction
}

object DetectorLike {
  val instances: Seq[Class[_]] = List(classOf[FASTDetector])

  implicit def fast = new DetectorLike[FASTDetector] {
    override def apply(detector: FASTDetector) = detector.apply
  }
}

object DetectorImpl {
  type DetectorAction = BufferedImage => Seq[KeyPoint]
}

sealed trait Detector

case class FASTDetector(val maxKeyPoints: Int) extends Detector {
  def apply(image: BufferedImage): Seq[KeyPoint] = {
    val detector = FeatureDetector.create("FAST")
    val matImage = OpenCVUtil.bufferedImageToCvMat(image)
    val keyPoints = new KeyPoint()
    detector.get().detect(matImage, keyPoints, null)
    val all = KeyPointUtil.keyPointsToList(keyPoints).sortBy(_.response).reverse
    all.take(maxKeyPoints)
  }
}
