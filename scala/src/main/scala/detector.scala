package nebula

import java.awt.image.BufferedImage
import org.opencv.core.MatOfKeyPoint
import org.opencv.features2d.{ DescriptorExtractor, FeatureDetector, KeyPoint }
import org.opencv.core.Mat

trait DetectorLike[T] {
  import DetectorImpl._

  def apply(detector: T): DetectorAction
}

object DetectorLike {
  val instances: Seq[Class[_]] = List(classOf[FASTDetector], classOf[BRISKDetector])

  implicit def fast = new DetectorLike[FASTDetector] {
    override def apply(detector: FASTDetector) = detector.apply
  }

  implicit def brisk = new DetectorLike[BRISKDetector] {
    override def apply(detector: BRISKDetector) = detector.apply
  }
}

object DetectorImpl {
  type DetectorAction = BufferedImage => Seq[KeyPoint]

  def apply(maxKeyPoints: Int, detector: FeatureDetector): DetectorAction =
    (image: BufferedImage) => {
      val matImage = OpenCVUtil.bufferedImageToMat(image)
      val keyPoints = new MatOfKeyPoint
      detector.detect(matImage, keyPoints)
      keyPoints.toArray.sortBy(_.response).reverse.take(maxKeyPoints)
    }
}

sealed trait Detector

case class FASTDetector(val maxKeyPoints: Int) extends Detector {
  import DetectorImpl._

  def apply: DetectorAction = DetectorImpl(
    maxKeyPoints,
    FeatureDetector.create(FeatureDetector.FAST))
}

case class BRISKDetector(val maxKeyPoints: Int) extends Detector {
  //  import DetectorImpl._
  //
  //  def apply: DetectorAction = (image) => {
  //    val detector = FeatureDetector.create(FeatureDetector.BRISK)
  //    val matImage = OpenCVUtil.bufferedImageToMat(image)
  //    val keyPoints = new MatOfKeyPoint
  //    detector.detect(matImage, keyPoints)
  //    
  //    // We need to run the extractor to get the angles.
  //    val extractor = DescriptorExtractor.create(DescriptorExtractor.BRISK)
  //    val descriptors = new Mat
  //    extractor.compute(matImage, keyPoints, descriptors)
  //    println(keyPoints.toArray.toList)
  //    
  //    keyPoints.toArray.sortBy(_.response).reverse.take(maxKeyPoints)
  //  }
  import DetectorImpl._

  def apply: DetectorAction = DetectorImpl(
    maxKeyPoints,
    FeatureDetector.create(FeatureDetector.BRISK))
}
