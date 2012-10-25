package nebula

import graveyard._
import mpie._
import summary._
import smallBaseline._
import util._
import util.imageProcessing._
import wideBaseline._

import java.awt.image.BufferedImage
import org.opencv.core.MatOfKeyPoint
import org.opencv.features2d.{ DescriptorExtractor, FeatureDetector, KeyPoint }
import org.opencv.core.Mat

sealed trait Detector {
  import Detector._

  def detect: DetectorAction
}

object Detector {
  val instances: Seq[Class[_]] = List(classOf[FASTDetector], classOf[BRISKDetector])
  
  type DetectorAction = BufferedImage => Seq[KeyPoint]

  def apply(maxKeyPoints: Option[Int], detector: FeatureDetector): DetectorAction =
    (image: BufferedImage) => {
      val matImage = OpenCVUtil.bufferedImageToMat(image)
      val keyPoints = new MatOfKeyPoint
      detector.detect(matImage, keyPoints)
      val sorted = keyPoints.toArray.sortBy(_.response).reverse
      
      if (maxKeyPoints.isDefined) sorted.take(maxKeyPoints.get)
      else sorted
    }
}

// TODO: Pimp
case class DenseDetector() extends Detector {
  import Detector._

  def detect: DetectorAction = Detector(
    None,
    FeatureDetector.create(FeatureDetector.DENSE))  
}

case class FASTDetector(maxKeyPoints: Int) extends Detector {
  import Detector._

  def detect: DetectorAction = Detector(
    Some(maxKeyPoints),
    FeatureDetector.create(FeatureDetector.FAST))
}

case class BRISKDetector(maxKeyPoints: Int) extends Detector {
  import Detector._

  def detect: DetectorAction = Detector(
    Some(maxKeyPoints),
    FeatureDetector.create(FeatureDetector.BRISK))
}
