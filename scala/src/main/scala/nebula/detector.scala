package nebula

import java.awt.image.BufferedImage

import org.opencv.core.MatOfKeyPoint
import org.opencv.features2d.{ FeatureDetector, KeyPoint }

import nebula.util.JSONUtil._
import spray.json.{ DefaultJsonProtocol, DeserializationException, JsString, JsValue, RootJsonFormat, pimpAny }
import util.JSONUtil.enumeration
import util.OpenCVUtil
import util._

///////////////////////////////////////////////////////////

trait Detector extends HasOriginal {
  def detect: Detector.DetectorAction
}

object Detector {
  type DetectorAction = BufferedImage => Seq[KeyPoint]
}

///////////////////////////////////////////////////////////

object OpenCVDetectorType extends Enumeration {
  type OpenCVDetectorType = Value
  val Dense, FAST, BRISK, SIFT, SURF = Value
}

case class OpenCVDetector(
  detectorType: OpenCVDetectorType.OpenCVDetectorType,
  maxKeyPointsOption: Option[Int])

object OpenCVDetector {
  import OpenCVDetectorType._

  implicit def implicitOpenCVDetector(self: OpenCVDetector): Detector =
    new Detector {
      override def detect = (image: BufferedImage) => {
        val detectorType = self.detectorType match {
          case Dense => FeatureDetector.DENSE
          case FAST => FeatureDetector.FAST
          case BRISK => FeatureDetector.BRISK
          case SIFT => FeatureDetector.SIFT
          case SURF => FeatureDetector.SURF
        }

        val matImage = OpenCVUtil.bufferedImageToMat(image)
        val keyPoints = new MatOfKeyPoint
        FeatureDetector.create(detectorType).detect(matImage, keyPoints)
        val sorted = keyPoints.toArray.sortBy(_.response).reverse

        if (self.maxKeyPointsOption.isDefined)
          sorted.take(self.maxKeyPointsOption.get)
        else sorted
      }

      override def original = self
    }
}

///////////////////////////////////////////////////////////

trait PairDetector extends Detector {
  def detectPair: PairDetector.PairDetectorAction
}

object PairDetector {
  type PairDetectorAction = (Homography, BufferedImage, BufferedImage) => Seq[Tuple2[KeyPoint, KeyPoint]]
}

///////////////////////////////////////////////////////////

case class OpenCVPairDetector(
  detector: OpenCVDetector,
  maxKeyPointsOption: Option[Int])

object OpenCVPairDetector {
  implicit def implicitOpenCVPairDetector(self: OpenCVPairDetector): PairDetector =
    new PairDetector {
      override def detect = self.detector.detect

      override def detectPair = (homography: Homography, leftImage: BufferedImage, rightImage: BufferedImage) => {
        val left = detect(leftImage)
        val right = detect(rightImage)

        // Euclidean distance in pixels.
        // TODO: Make parameter
        val threshold = 2

        val allPairs = Util.nearestUnderWarpRemoveDuplicates(
          threshold,
          homography,
          left,
          right).sortBy(KeyPointUtil.pairQuality).reverse

        if (self.maxKeyPointsOption.isDefined)
          allPairs.take(self.maxKeyPointsOption.get)
        else allPairs
      }

      override def original = self
    }
}

///////////////////////////////////////////////////////////

object DetectorJsonProtocol extends DefaultJsonProtocol {
  implicit val openCVDetectorType = enumeration(
    "OpenCVDetectorType",
    Map(
      "Dense" -> OpenCVDetectorType.Dense,
      "FAST" -> OpenCVDetectorType.FAST,
      "BRISK" -> OpenCVDetectorType.BRISK,
      "SIFT" -> OpenCVDetectorType.SIFT,
      "SURF" -> OpenCVDetectorType.SURF))

  implicit val openCVDetector =
    jsonFormat2(OpenCVDetector.apply).addClassInfo("OpenCVDetector")

  /////////////////////////////////////////////////////////

  implicit object DetectorJsonFormat extends RootJsonFormat[Detector] {
    override def write(self: Detector) = self.original match {
      case original: OpenCVDetector => original.toJson
    }
    override def read(value: JsValue) = value.asJsObject.fields("scalaClass") match {
      case JsString("OpenCVDetector") => value.convertTo[OpenCVDetector]
      case _ => throw new DeserializationException("Detector expected")
    }
  }
}

///////////////////////////////////////////////////////////

object PairDetectorJsonProtocol extends DefaultJsonProtocol {
  import DetectorJsonProtocol._

  implicit val openCVPairDetector =
    jsonFormat2(OpenCVPairDetector.apply).addClassInfo("OpenCVPairDetector")

  /////////////////////////////////////////////////////////    

  implicit object PairDetectorJsonFormat extends RootJsonFormat[PairDetector] {
    override def write(self: PairDetector) = self.original match {
      case original: OpenCVPairDetector => original.toJson
    }
    override def read(value: JsValue) = value.asJsObject.fields("scalaClass") match {
      case JsString("OpenCVPairDetector") => value.convertTo[OpenCVPairDetector]
      case _ => throw new DeserializationException("PairDetector expected")
    }
  }
}