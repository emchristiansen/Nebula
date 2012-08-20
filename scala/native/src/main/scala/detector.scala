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

  implicit def brisk = new DetectorLike[BRISKDetector] {
    override def apply(detector: BRISKDetector) = detector.apply
  }
}

object DetectorImpl {
  type DetectorAction = BufferedImage => Seq[KeyPoint]

  def apply(maxKeyPoints: Int, detector: FeatureDetector): DetectorAction = (image: BufferedImage) => {
    val matImage = OpenCVUtil.bufferedImageToCvMat(image)
    val keyPoints = new KeyPoint()
    detector.detect(matImage, keyPoints, null)
    val all = KeyPointUtil.keyPointsToSeq(keyPoints).sortBy(_.response).reverse
    all.take(maxKeyPoints)
  }
}

sealed trait Detector

case class FASTDetector(val maxKeyPoints: Int) extends Detector {
  def apply = DetectorImpl(
    maxKeyPoints,
    new FastFeatureDetector(10, true))
}

case class BRISKDetector(val maxKeyPoints: Int) extends Detector {
  def apply = DetectorImpl(
    maxKeyPoints,
    new BriskFeatureDetector(10, 4))
}
