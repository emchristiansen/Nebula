import org.scalatest.FunSuite
import org.opencv.features2d._
import javax.imageio.ImageIO
import java.io.File
import org.opencv.core.MatOfKeyPoint
import org.opencv.features2d.{ FeatureDetector, KeyPoint }
import nebula._
//import org.apache.xmlgraphics.image.loader.ImageManager
import org.opencv.core.Mat
import org.opencv.core.MatOfKeyPoint
import nebula.util.JSONUtil
import spray.json.DefaultJsonProtocol
import spray.json.JsArray
import spray.json.RootJsonFormat
import spray.json.JsValue
import spray.json.JsNumber
import spray.json.JsString

import wideBaseline._
import smallBaseline._

import spray.json._

import nebula.util.Memoize._
import nebula.util._

import breeze.linalg._

import DetectorJsonProtocol._
import ExtractorJsonProtocol._
import MatcherJsonProtocol._
import ExperimentJsonProtocol._
import ExperimentResultsJsonProtocol._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import TestUtil._

import org.scalatest.FunSuite
import org.opencv.features2d._
import javax.imageio.ImageIO
import java.io.File
import org.opencv.core.MatOfKeyPoint
import org.opencv.features2d.{ FeatureDetector, KeyPoint }
import nebula._
//import org.apache.xmlgraphics.image.loader.ImageManager
import org.opencv.core.Mat
import java.awt.Color
import java.awt.image.BufferedImage
import org.apache.commons.math3.linear.Array2DRowRealMatrix
import nebula.util.Homography
import nebula.util.OpenCVUtil
import nebula.util.KeyPointUtil

import javax.imageio.ImageIO

import java.awt.{ Color, Rectangle }
import java.awt.color.ColorSpace
import java.awt.geom.AffineTransform
import java.awt.image.{ AffineTransformOp, BufferedImage, ColorConvertOp, ConvolveOp, DataBufferInt, Kernel }

import nebula.graveyard._
import nebula.mpie._
import nebula.summary._
import nebula.smallBaseline._
import nebula.util._
import nebula.util.imageProcessing._
import nebula.wideBaseline._
import nebula._

import scala.Array.{ canBuildFrom, fallbackCanBuildFrom }

import org.opencv.features2d.KeyPoint

import java.awt.image.AffineTransformOp.TYPE_BILINEAR

import breeze.linalg.DenseMatrix

import org.opencv.features2d.{ DMatch, KeyPoint }

import DenseMatrixUtil._

import TestUtil._

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
class TestAnything extends FunSuite {
  val image = ImageIO.read(new File(
    getClass.getResource("/iSpy.jpg").getFile))  
  
  test("blah") {
    
    val patchWidth = math.min(image.getWidth, image.getHeight)
    val imageCenter = (image.getWidth / 2, image.getHeight / 2)

    val leftPatch = image.getSubimage(
      imageCenter._1 - patchWidth / 2,
      imageCenter._2 - patchWidth / 2,
      patchWidth,
      patchWidth)

    dumpImage("leftPatch", leftPatch)

    val rotate = new AffineTransformOp(
      AffineTransform.getRotateInstance(
        math.Pi / 2,
        patchWidth / 2,
        patchWidth / 2),
      TYPE_BILINEAR)
    val scaleFactor = 2
    val scale = new AffineTransformOp(
      AffineTransform.getScaleInstance(scaleFactor, scaleFactor),
      TYPE_BILINEAR)

    val rightPatch = scale.filter(rotate.filter(leftPatch, null), null)

    dumpImage("rightPatch", rightPatch)

    val extractor = LogPolarExtractor(
      PatchExtractorType.Rank,
      false,
      true,
      1,
      patchWidth / 2 - 1,
      64,
      64,
      3,
      "Gray")

    val leftKeyPoint = new KeyPoint(patchWidth / 2, patchWidth / 2, 0)
    val rightKeyPoint = new KeyPoint(
      scaleFactor * patchWidth / 2,
      scaleFactor * patchWidth / 2, 0)
    println(leftKeyPoint)
    println(rightKeyPoint)
    println(extractor)

//    val leftDescriptor =
//      extractor.extractSingle(leftPatch, leftKeyPoint).get.original.asInstanceOf[DenseMatrix[Double]]
//    val rightDescriptor =
//      extractor.extractSingle(rightPatch, rightKeyPoint).get.original.asInstanceOf[DenseMatrix[Double]]
//    
//    dumpImage("leftDescriptor", leftDescriptor.toScaledImage)
//    dumpImage("rightDescriptor", rightDescriptor.toScaledImage)
//
//    val leftPadded = LogPolarMatcher.prepareMatrixForConvolution(leftDescriptor)
//
//    dumpImage("leftPadded", leftPadded.toScaledImage)
//
//    val matcherType = MatcherType.L1
//
//    import MatcherType._
//    import Matcher._
//    val correlationDistance = matcherType match {
//      case L1 => l1[Double] _
//      case L2 => l2[Double] _
//      case _ => sys.error("Not using supported distance")
//    }
//
//    val unnormalizedResponse = LogPolarMatcher.getResponseMap(
//          false, 
//          correlationDistance,
//          leftDescriptor, 
//          rightDescriptor)
//    //    val normalizedResponse = 
//    //      LogPolarMatcher.getResponseMap(true, leftDescriptor, rightDescriptor)
//
//    def highlight(x: Double) = -1 * x
//
//    dumpImage(
//      "unnormalizedResponse",
//      unnormalizedResponse.map(highlight).toScaledImage)
//    //    dumpImage(
//    //        "normalizedResponse", 
//    //        normalizedResponse.map(highlight).toScaledImage)        
//
//    println("unnormalizedResponse max and argmax: %s, %s".format(
//      unnormalizedResponse.max,
//      unnormalizedResponse.argmax))
//
//    //    println("normalizedResponse max and argmax: %s, %s".format(
//    //        normalizedResponse.max, 
//    //        normalizedResponse.argmax))
  }
}

//    import tools.nsc.interpreter.ProductCompletion
//
//    case class Person(name: String, age: Int)
//
//    val bob = Person("Bob", 20)
//
//    def toSource(p: Product): String =
//      p.productIterator.map {
//        case s: String => "\"" + s + "\""
//        case p: Product => toSource(p)
//        case other => other.toString
//      } mkString (p.productPrefix + "(", ", ", ")")
//
//    type BlahT[A] = A => Blah2Ops
//
//    trait Blah2Ops {
//      def blah2: Int
//    }
//
//    implicit def toBlah2Ops[A: BlahT](self: A): Blah2Ops =
//      implicitly[BlahT[A]].apply(self)
//
//    def doBlah[A: BlahT](blahable: A) {
//      blahable.blah2
//    }
//
//    class Color(val name: String, val red: Int, val green: Int, val blue: Int)
//
//    object WeekDay extends Enumeration {
//      type WeekDay = Value
//      val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
//    }
//
//    object MyJsonProtocol extends DefaultJsonProtocol {
//      implicit object ColorJsonFormat extends RootJsonFormat[Color] {
//        def write(c: Color) =
//          JsArray(JsString(c.name), JsNumber(c.red), JsNumber(c.green), JsNumber(c.blue))
//
//        def read(value: JsValue) = value match {
//          case JsArray(JsString(name) :: JsNumber(red) :: JsNumber(green) :: JsNumber(blue) :: Nil) =>
//            new Color(name, red.toInt, green.toInt, blue.toInt)
//          case _ => TODO
//        }
//      }
//
//      implicit object WeekDayJsonFormat extends RootJsonFormat[WeekDay.WeekDay] {
//        import WeekDay._
//
//        def write(w: WeekDay) = w match {
//          case Mon => JsString("Mon")
//          case _ => TODO
//        }
//
//        def read(value: JsValue) = value match {
//          case JsString("Mon") => WeekDay.Mon
//          case _ => TODO
//        }
//      }
//    }
//
//    import MyJsonProtocol._
//
//    val json = new Color("CadetBlue", 95, 158, 160).toJson
//
//    println(json)
//
//    println(("hello", 42).toJson)
//
//    println(WeekDay.Mon.toJson.convertTo[WeekDay.WeekDay])
//
//    val color = json.convertTo[Color]
//
//    import OpenCVDetectorType._
//    import DetectorJsonProtocol._
//
//    println(OpenCVDetector(Dense, Some(100)).toJson.prettyPrint)
//
//    val json2 = OpenCVDetector(Dense, Some(100)).toJson
//    println(json2)
//    println(json2.convertTo[OpenCVDetector])
//
//    import PatchExtractorType._
//    import ExtractorJsonProtocol._
//    import MatcherJsonProtocol._
//    import ExperimentJsonProtocol._
//
//    val json3 = PatchExtractor(Raw, true, false, 8, 4, "sRGB").toJson
//    println(json3)
//    println(json3.convertTo[Extractor])
//
//    val json4 = MatcherType.L1.toJson
//    println(json4)
//    println(json4.convertTo[MatcherType.MatcherType])
//
//    val wide = WideBaselineExperiment(
//      "bikes",
//      4,
//      OpenCVDetector(BRISK, Some(100)),
//      BRISKExtractor(BRISKExtractorType.Rank, false, true),
//      MatcherType.L0)
//    
//    val json5 = wide.toJson
//
//    println(json5.prettyPrint)
//    println(json5.convertTo[Experiment])
//    
//    val detector = OpenCVDetector(Dense, Some(100))
//    val extractor = PatchExtractor(Raw, true, false, 8, 4, "sRGB")
//    val matcher = MatcherType.L1
//    
//    println(JSONUtil.abbreviate(detector))
//    println(JSONUtil.abbreviate(extractor))
//    println(JSONUtil.abbreviate(matcher))
//    println(wide.parameters)
//      
//    val x = () => {
//      println("Should only be printed once")
//      4
//    }
//    
//    val asdf = Memoize(x)
//    println(asdf())
//    println(asdf())
//    
//    //    val asdf = json2.convertTo[Detector]
