package nebula

import java.awt.image.BufferedImage

import org.opencv.core.MatOfKeyPoint
import org.opencv.features2d.{ FeatureDetector, KeyPoint }

import nebula.util.JSONUtil._
import spray.json._
import util.OpenCVUtil
import util._

///////////////////////////////////////////////////////////

trait Detector {
  def detect: Detector.DetectorAction
}

object Detector {
  type DetectorAction = BufferedImage => Seq[KeyPoint]
}

///////////////////////////////////////////////////////////

object OpenCVDetectorType {
  object DENSE
  object FAST
  object BRISK
  object SIFT
  object SURF

  def detectorFromEnum(detectorType: Int): Detector = new Detector {
    override def detect = image => {
      val matImage = OpenCVUtil.bufferedImageToMat(image)
      val keyPoints = new MatOfKeyPoint
      FeatureDetector.create(detectorType).detect(matImage, keyPoints)
      keyPoints.toArray.sortBy(_.response).reverse
    }
  }

  implicit def detector(self: DENSE.type) =
    detectorFromEnum(FeatureDetector.DENSE)
  implicit def detector(self: FAST.type) =
    detectorFromEnum(FeatureDetector.FAST)
  implicit def detector(self: BRISK.type) =
    detectorFromEnum(FeatureDetector.BRISK)
  implicit def detector(self: SIFT.type) =
    detectorFromEnum(FeatureDetector.SIFT)
  implicit def detector(self: SURF.type) =
    detectorFromEnum(FeatureDetector.SURF)
}

case class BoundedDetector[D <% Detector](detector: D, maxKeyPoints: Int)

object BoundedDetector {
  implicit class ToDetector[D <% Detector](self: BoundedDetector[D]) extends Detector {
    override def detect = image => self.detector.detect(image).take(self.maxKeyPoints)
  }
}

///////////////////////////////////////////////////////////

trait PairDetector extends Detector {
  def detectPair: PairDetector.PairDetectorAction
}

object PairDetector {
  type PairDetectorAction = (Homography, BufferedImage, BufferedImage) => Seq[Tuple2[KeyPoint, KeyPoint]]

  implicit class ToPairDetector[D <% Detector](self: D) extends PairDetector {
    override def detect = self.detect

    override def detectPair = (homography: Homography, leftImage: BufferedImage, rightImage: BufferedImage) => {
      val left = detect(leftImage)
      val right = detect(rightImage)

      // Euclidean distance in pixels.
      // TODO: Make parameter
      val threshold = 2

      Util.nearestUnderWarpRemoveDuplicates(
        threshold,
        homography,
        left,
        right).sortBy(KeyPointUtil.pairQuality).reverse
    }
  }
  
  // This enumeration is necessary because Scala doesn't do deep searches for implicits.
  implicit def detector(self: OpenCVDetectorType.DENSE.type) = self.to[Detector].to[PairDetector]
  implicit def detector(self: OpenCVDetectorType.FAST.type) = self.to[Detector].to[PairDetector]
  implicit def detector(self: OpenCVDetectorType.BRISK.type) = self.to[Detector].to[PairDetector]
  implicit def detector(self: OpenCVDetectorType.SIFT.type) = self.to[Detector].to[PairDetector]
  implicit def detector(self: OpenCVDetectorType.SURF.type) = self.to[Detector].to[PairDetector]
}

case class BoundedPairDetector[D <% PairDetector](pairDetector: D, maxKeyPoints: Int)

object BoundedPairDetector { 
  implicit class ToPairDetector[D <% PairDetector](self: BoundedPairDetector[D]) extends Detector {
    override def detect = image => self.pairDetector.detect(image).take(self.maxKeyPoints)
  }
}

///////////////////////////////////////////////////////////

object DetectorJsonProtocol extends DefaultJsonProtocol {
  implicit val dense = singletonObject(OpenCVDetectorType.DENSE)
  implicit val fast = singletonObject(OpenCVDetectorType.FAST)
  implicit val brisk = singletonObject(OpenCVDetectorType.BRISK)
  implicit val sift = singletonObject(OpenCVDetectorType.SIFT)
  implicit val surf = singletonObject(OpenCVDetectorType.SURF)

  /////////////////////////////////////////////////////////
  
  implicit def boundedDetector[D <% Detector : JsonFormat] = jsonFormat2(BoundedDetector.apply[D])

  /////////////////////////////////////////////////////////  
  
  implicit def boundedPairDetector[D <% PairDetector : JsonFormat] = jsonFormat2(BoundedPairDetector.apply[D])
}