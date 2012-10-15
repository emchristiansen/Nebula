package nebula

import java.io.File
import xml._

import org.opencv.core.Mat
import org.opencv.features2d.{ DMatch, KeyPoint }
import org.opencv.highgui.Highgui.imread

import DetectorImpl._
import ExtractorImpl._
import MatcherImpl._

case class PredictionAndTruth(val prediction: Double, val truth: Boolean)

case class ResultsData(val predictionsAndTruths: Seq[PredictionAndTruth])

object ResultsData {
  def sorted(predictions: Seq[Double], truths: Seq[Boolean]): ResultsData = {
    val sortedPairs = predictions.zip(truths).sortBy(_._1)
    val predictionsAndTruths =
      sortedPairs.map(e => PredictionAndTruth(e._1, e._2))
    ResultsData(predictionsAndTruths)
  }
}

trait ExperimentResults {
  def experiment: Experiment
  def save: Unit
  
  def run: this.type
  
  ///////////////////////////////////////////////////////////
  
  val unixEpoch = System.currentTimeMillis / 1000L

  def stringMap = parameters.toMap
  
  def filenameNoTime: String =
    name + "_" + parameters.map(p => p._1 + "-" + p._2).mkString("_") + ".json"

  def filename: String = unixEpoch + "_" + filenameNoTime

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

// TODO
case class SmallBaselineExperimentResults(
  val experiment: SmallBaselineExperiment,
  val dmatches: Seq[DMatch])

case class WideBaselineExperimentResults(
  val experiment: WideBaselineExperiment,
  val dmatches: Seq[DMatch])

object WideBaselineExperimentResults {
  def apply(
    experiment: WideBaselineExperiment): WideBaselineExperimentResults = {
    if (experiment.alreadyRun) {
      val Some(file) = experiment.existingResultsFile
      println("Reading %s".format(file))
      IO.fromJSONFileAbstract[WideBaselineExperimentResults](ExperimentIO.formats, file)
    } else experiment.run
  }  
  
  implicit def implicitExperimentResults(self: WideBaselineExperimentResults): ExperimentResults =
    new ExperimentResults {
      override def experiment = self.experiment
      override def save = {
        println("Writing to %s".format(experiment.path))
        IO.toJSONFileAbstract(
          ExperimentIO.formats,
          WideBaselineExperimentResults.this,
          experiment.path)
      }
    }
}

