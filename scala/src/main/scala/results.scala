package nebula

import java.io.File
import xml._

import breeze.linalg._

import org.opencv.core.Mat
import org.opencv.features2d.{ DMatch, KeyPoint }
import org.opencv.highgui.Highgui.imread

import DetectorImpl._
import ExtractorImpl._
import MatcherImpl._

///////////////////////////////////////////////////////////

case class PredictionAndTruth(val prediction: Double, val truth: Boolean)

///////////////////////////////////////////////////////////

case class ResultsData(val predictionsAndTruths: Seq[PredictionAndTruth])

object ResultsData {
  def sorted(predictions: Seq[Double], truths: Seq[Boolean]): ResultsData = {
    val sortedPairs = predictions.zip(truths).sortBy(_._1)
    val predictionsAndTruths =
      sortedPairs.map(e => PredictionAndTruth(e._1, e._2))
    ResultsData(predictionsAndTruths)
  }
}

///////////////////////////////////////////////////////////

trait ExperimentResults {
  def experiment: Experiment
  def save: Unit

  ///////////////////////////////////////////////////////////

  def filenameNoTime: String =
    experiment.name + "_" + experiment.parameters.map(p => p._1 + "-" + p._2).mkString("_") + ".json"

  def filename: String = experiment.unixEpoch + "_" + filenameNoTime

  def outDirectory: File = Global.run[RuntimeConfig].projectChildPath("results/experiment_data")

  def path: File = new File(outDirectory, filename)

  def existingResultsFiles: Seq[File] = {
    val allPaths = outDirectory.list.toList.map(path => outDirectory + "/" + path.toString)
    val matchingPaths = allPaths.filter(_.contains(filenameNoTime))
    matchingPaths.sortBy(identity).reverse.map(path => new File(path))
  }

  def existingResultsFile: Option[File] = {
    existingResultsFiles match {
      case Nil => None
      case file :: _ => Some(file)
    }
  }

  def alreadyRun: Boolean = !existingResultsFile.isEmpty
}

object ExperimentResults {
  def apply(experiment: Experiment): ExperimentResults = experiment.original match {
    case original: WideBaselineExperiment => WideBaselineExperimentResults(original)
    case original: SmallBaselineExperiment => SmallBaselineExperimentResults(original)
    case _ => sys.error("")
  }
}





