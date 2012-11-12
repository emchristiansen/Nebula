package nebula

import java.io.File

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
  def save: Unit

  def experimentStringNoTime = experiment.name + "_" + experiment.parameters.map(
    p => p._1 + "-" + p._2).mkString("_")

  def experimentString = experiment.unixEpoch + "_" + experimentStringNoTime

  def filenameNoTime: String = experimentStringNoTime + ".json"

  def filename: String = experiment.unixEpoch + "_" + filenameNoTime

  def outDirectory: File = Global.run[RuntimeConfig].projectChildPath(
    "results/experiment_data/")

  def path: File = new File(outDirectory, filename)

  def existingResultsFiles: Seq[File] = {
    val allPaths = outDirectory.list.toList.map(path => outDirectory + "/" + path.toString)
    val matchingPaths = allPaths.filter(_.contains(filenameNoTime))
    matchingPaths.sortBy(identity).reverse.map(path => new File(path))
  }

  def existingResultsFile: Option[File] = existingResultsFiles.headOption

  def alreadyRun: Boolean = !existingResultsFile.isEmpty
}

object ExperimentResults {
  def apply(experiment: Experiment): ExperimentResults = experiment.original match {
    case original: WideBaselineExperiment => WideBaselineExperimentResults(original)
    case original: SmallBaselineExperiment => SmallBaselineExperimentResults(original)
  }
}

///////////////////////////////////////////////////////////

object ExperimentResultsJsonProtocol extends DefaultJsonProtocol {
  implicit val wideBaselineExperimentResults =
    jsonFormat2(WideBaselineExperimentResults.apply).addClassInfo(
      "WideBaselineExperimentResults")

  implicit val smallBaselineExperimentResults =
    jsonFormat3(SmallBaselineExperimentResults.apply).addClassInfo(
      "SmallBaselineExperimentResults")

  implicit object ExperimentResultsJsonProtocol extends RootJsonFormat[ExperimentResults] {
    override def write(self: ExperimentResults) = self.original match {
      case original: WideBaselineExperimentResults => original.toJson
      case original: SmallBaselineExperimentResults => original.toJson
    }
    override def read(value: JsValue) = value.asJsObject.fields("scalaClass") match {
      case JsString("WideBaselineExperimentResults") =>
        value.convertTo[WideBaselineExperimentResults]
      case JsString("SmallBaselineExperimentResults") =>
        value.convertTo[SmallBaselineExperimentResults]
      case _ => throw new DeserializationException("ExperimentResults expected")
    }
  }
}



