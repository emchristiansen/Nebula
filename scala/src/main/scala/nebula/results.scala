package nebula

import java.io.File
import nebula.summary._

import nebula.smallBaseline.SmallBaselineExperimentResults.implicitExperimentResults
import nebula.wideBaseline.WideBaselineExperimentResults.implicitExperimentResults
import smallBaseline.{ SmallBaselineExperiment, SmallBaselineExperimentResults }
import wideBaseline.{ WideBaselineExperiment, WideBaselineExperimentResults }

import spray.json._
import util.JSONUtil._
import util._

import ExperimentJsonProtocol._
import DMatchJsonProtocol._
import smallBaseline.FlowFieldJsonProtocol._

///////////////////////////////////////////////////////////

trait ExperimentResults extends HasOriginal {
  def experiment: Experiment
  def save(implicit runtime: RuntimeConfig): Unit

  def toSummary(implicit runtime: RuntimeConfig): ExperimentSummary
  
  def experimentStringNoTime = {
    val fullString = JSONUtil.flattenJson(experiment.toJson)
    // Unfortunately, this string is too long to be part of a filename.
    fullString.take(100) + "_" + fullString.hashCode
  }
    //experiment.name + "_" + experiment.parameters.map(    p => p._1 + "-" + p._2).mkString("_")

  def experimentString = experiment.unixEpoch + "_" + experimentStringNoTime

  def filenameNoTime: String = experimentStringNoTime + ".json"

  def filename: String = experiment.unixEpoch + "_" + filenameNoTime

  def outDirectory(implicit runtime: RuntimeConfig): File = runtime.projectChildPath(
    "results/experiment_data/")

  def path(implicit runtime: RuntimeConfig): File = new File(outDirectory, filename)

  def existingResultsFiles(implicit runtime: RuntimeConfig): Seq[File] = {
    val allPaths = outDirectory.list.toList.map(path => outDirectory + "/" + path.toString)
    val matchingPaths = allPaths.filter(_.contains(filenameNoTime))
    matchingPaths.sortBy(identity).reverse.map(path => new File(path))
  }

  def existingResultsFile(implicit runtime: RuntimeConfig): Option[File] = existingResultsFiles.headOption

  def alreadyRun(implicit runtime: RuntimeConfig): Boolean = !existingResultsFile.isEmpty
}

//object ExperimentResults {
//  def apply(experiment: Experiment): ExperimentResults = experiment.original match {
//    case original: WideBaselineExperiment => WideBaselineExperimentResults(original)
//    case original: SmallBaselineExperiment => SmallBaselineExperimentResults(original)
//  }
//}

///////////////////////////////////////////////////////////

object ExperimentResultsJsonProtocol extends DefaultJsonProtocol {
  implicit val smallBaselineExperimentResults: RootJsonFormat[SmallBaselineExperimentResults] =
    jsonFormat3(SmallBaselineExperimentResults.apply).addClassInfo(
      "SmallBaselineExperimentResults")  
  
  implicit val wideBaselineExperimentResults =
    jsonFormat2(WideBaselineExperimentResults.apply).addClassInfo(
      "WideBaselineExperimentResults")

//  implicit object ExperimentResultsJsonProtocol extends RootJsonFormat[ExperimentResults] {
//    override def write(self: ExperimentResults) = self.original match {
//      case original: WideBaselineExperimentResults => original.toJson
//      case original: SmallBaselineExperimentResults => original.toJson
//    }
//    override def read(value: JsValue) = value.asJsObject.fields("scalaClass") match {
//      case JsString("WideBaselineExperimentResults") =>
//        value.convertTo[WideBaselineExperimentResults]
//      case JsString("SmallBaselineExperimentResults") =>
//        value.convertTo[SmallBaselineExperimentResults]
//      case _ => throw new DeserializationException("ExperimentResults expected")
//    }
//  }
}



