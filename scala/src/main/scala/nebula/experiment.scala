package nebula

import java.awt.image.BufferedImage

import nebula.util.JSONUtil._

import spray.json.{DefaultJsonProtocol, DeserializationException, JsString, JsValue, RootJsonFormat, pimpAny}
import wideBaseline.WideBaselineExperiment

///////////////////////////////////////////////////////////

trait HasImagePair {
  def leftImage: BufferedImage
  def rightImage: BufferedImage
}

///////////////////////////////////////////////////////////

trait HasGroundTruth[A] {
  def groundTruth: A
}

///////////////////////////////////////////////////////////

trait HasEstimate[A] {
  def estimate: A
}

///////////////////////////////////////////////////////////

//trait Experiment {
////  def name: String
////
////  // Parameter names and values
////  def parameters: Seq[Tuple2[String, String]]
//  
//  def getResults(implicit runtime: RuntimeConfig): ExperimentResults
//  
//  /////////////////////////////////////////////////////////// 
//  
//  val unixEpoch = System.currentTimeMillis / 1000L
//
////  def stringMap = parameters.toMap
//}

///////////////////////////////////////////////////////////

//object ExperimentJsonProtocol extends DefaultJsonProtocol {
////  import DetectorJsonProtocol._
////  import ExtractorJsonProtocol._
////  import MatcherJsonProtocol._
////  
////  implicit val wideBaselineExperiment =
////    jsonFormat5(WideBaselineExperiment.apply).addClassInfo("WideBaselineExperiment")
////    
////  implicit val smallBaselineExperiment =
////    jsonFormat4(SmallBaselineExperiment.apply).addClassInfo("SmallBaselineExperiment")
//    
////  implicit object ExperimentJsonFormat extends RootJsonFormat[Experiment] {
////    override def write(self: Experiment) = self.original match {
////      case original: WideBaselineExperiment => original.toJson
////      case original: SmallBaselineExperiment => original.toJson
////    }
////    
////    override def read(value: JsValue) = value.asJsObject.fields("scalaClass") match {
////      case JsString("WideBaselineExperiment") => value.convertTo[WideBaselineExperiment]
////      case JsString("SmallBaselineExperiment") => value.convertTo[SmallBaselineExperiment]
////      case _ => throw new DeserializationException("Experiment expected")
////    }
////  }
//}