import org.scalatest.FunSuite
import org.opencv.features2d._
import javax.imageio.ImageIO
import java.io.File
import org.opencv.core.MatOfKeyPoint
import org.opencv.features2d.{ FeatureDetector, KeyPoint }
import nebula._
import org.apache.xmlgraphics.image.loader.ImageManager
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

class TestAnything extends FunSuite {
  test("blah") {
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
  }
}
