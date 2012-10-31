package nebula

import java.awt.image.BufferedImage
import org.opencv.core.MatOfKeyPoint
import org.opencv.features2d.{ FeatureDetector, KeyPoint }
import util.{ JSONSerializable, JSONUtil, OpenCVUtil }
import net.liftweb.json.Serializer
import net.liftweb.json.Formats
import net.liftweb.json.MappingException
import org.opencv.features2d.DMatch
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.TypeInfo
import net.liftweb.json.JsonAST.JField
import net.liftweb.json.JsonAST.JDouble
import net.liftweb.json.JsonAST.JString
import net.liftweb.json.JsonAST.JInt
import net.liftweb.json.Serialization
import net.liftweb.json.ShortTypeHints
import spray.json.DefaultJsonProtocol

import spray.json.JsObject
import spray.json.RootJsonFormat
import spray.json.JsValue

import util._

import spray.json._

///////////////////////////////////////////////////////////

trait Detector extends HasOriginal with JSONSerializable {
  def detect: Detector.DetectorAction
}

object Detector {
  type DetectorAction = BufferedImage => Seq[KeyPoint]
}

///////////////////////////////////////////////////////////

object OpenCVDetectorType extends Enumeration {
  type OpenCVDetectorType = Value
  val Dense, FAST, BRISK = Value
}

import OpenCVDetectorType._

case class OpenCVDetector(
  detectorType: OpenCVDetectorType,
  maxKeyPointsOption: Option[Int])

object OpenCVDetector {
  //  val instances = List(
  //    classOf[OpenCVDenseDetector],
  //    classOf[OpenCVFASTDetector],
  //    classOf[OpenCVBRISKDetector])

  implicit def implicitOpenCVDetector(self: OpenCVDetector): Detector =
    new Detector {
      override def detect = (image: BufferedImage) => {
        val detectorType = self.detectorType match {
          case Dense => FeatureDetector.DENSE
          case FAST => FeatureDetector.FAST
          case BRISK => FeatureDetector.BRISK
        }

        val matImage = OpenCVUtil.bufferedImageToMat(image)
        val keyPoints = new MatOfKeyPoint
        FeatureDetector.create(detectorType).detect(matImage, keyPoints)
        val sorted = keyPoints.toArray.sortBy(_.response).reverse

        if (self.maxKeyPointsOption.isDefined) sorted.take(self.maxKeyPointsOption.get)
        else sorted
      }

      override def original = self

      override def json = JSONUtil.toJSON(self, Nil)
    }
}

///////////////////////////////////////////////////////////

object DetectorJsonProtocol extends DefaultJsonProtocol {
  implicit object OpenCVDetectorTypeJsonFormat extends EnumerationJsonFormat[OpenCVDetectorType] {
    override val expectedType = "OpenCVDetectorType"

    override val deserializeMapping = Map(
      "Dense" -> Dense,
      "FAST" -> FAST,
      "BRISK" -> BRISK)
  }

  implicit def implicitAddClassName[A](self: RootJsonFormat[A]) = new {
    def addClassInfo(scalaClass: String): RootJsonFormat[A] = new RootJsonFormat[A] {
      override def write(e: A) = {
        val fields = self.write(e).asJsObject.fields
        assert(!fields.contains("scalaClass"))
        JsObject(fields + ("scalaClass" -> JsString(scalaClass)))
      }
      override def read(value: JsValue) = self.read(value)
    }
  }

  implicit val openCVDetectorFormat = 
    jsonFormat2(OpenCVDetector.apply).addClassInfo("OpenCVDetector")

    implicit object DetectorJsonFormat extends RootJsonFormat[Detector] {
      override def write(self: Detector) = self.original match {
        case original: OpenCVDetector =>  original.toJson
      }
      override def read(value: JsValue) = value.asJsObject.fields("scalaClass") match {
        case JsString("OpenCVDetector") => value.convertTo[OpenCVDetector]
        case _ => throw new DeserializationException("Detector expected")
      } 
    }
}