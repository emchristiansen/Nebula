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
import net.liftweb.json.Serialization
import net.liftweb.json.ShortTypeHints

import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.Serialization
import net.liftweb.json.Formats
import net.liftweb.json.Serialization
import net.liftweb.json.Serialization.read
import net.liftweb.json.Serialization.write
import net.liftweb.json.ShortTypeHints
import net.liftweb.json.parse
import net.liftweb.json.pretty
import net.liftweb.json.render
import net.liftweb.json.JsonAST.JField
import net.liftweb.json.JsonAST.JString
import scala.text.{ Document, DocText }
import net.liftweb.json.JsonAST.JValue

///////////////////////////////////////////////////////////

trait Detector extends JSONSerializable {
  def detect: Detector.DetectorAction
}

object Detector {
  val instances: Seq[Class[_]] = nebula.TODO //List(classOf[FASTDetector], classOf[BRISKDetector])

  type DetectorAction = BufferedImage => Seq[KeyPoint]

  implicit def implicitOpenCVDetector(self: OpenCVDetector): Detector =
    new Detector {
      def detect = (image: BufferedImage) => {
        val detectorType = self.detectorType match {
          case OpenCVDenseDetector => FeatureDetector.DENSE
          case OpenCVFASTDetector => FeatureDetector.FAST
          case OpenCVBRISKDetector => FeatureDetector.BRISK
        }

        val matImage = OpenCVUtil.bufferedImageToMat(image)
        val keyPoints = new MatOfKeyPoint
        FeatureDetector.create(detectorType).detect(matImage, keyPoints)
        val sorted = keyPoints.toArray.sortBy(_.response).reverse

        if (self.maxKeyPointsOption.isDefined) sorted.take(self.maxKeyPointsOption.get)
        else sorted
      }

      def json = JSONUtil.toJSON(self)
    }
}

///////////////////////////////////////////////////////////

sealed trait OpenCVDetectorType

object OpenCVDenseDetector extends OpenCVDetectorType

object OpenCVFASTDetector extends OpenCVDetectorType

object OpenCVBRISKDetector extends OpenCVDetectorType

case class OpenCVDetector(
  detectorType: OpenCVDetectorType,
  maxKeyPointsOption: Option[Int])
