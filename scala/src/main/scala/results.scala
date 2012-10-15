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

case class SmallBaselineExperimentResults(
  val experiment: SmallBaselineExperiment,
  val dmatches: Seq[DMatch])
  
case class WideBaselineExperimentResults(
  val experiment: WideBaselineExperiment, 
  val dmatches: Seq[DMatch]) {
  def save {
    println("Writing to %s".format(experiment.path))
    IO.toJSONFileAbstract(
      ExperimentIO.formats,      WideBaselineExperimentResults.this,      experiment.path)
  }
}

object WideBaselineExperimentResults {
  def runExperiment(
    experiment: WideBaselineExperiment): WideBaselineExperimentResults = {

    println("Running %s".format(experiment))

    val leftImage = experiment.leftImage
    val rightImage = experiment.rightImage

    val (leftKeyPoints, rightKeyPoints) = {
      val leftKeyPoints = experiment.detector.detect(leftImage)
      Util.pruneKeyPoints(
        leftImage,
        rightImage,
        experiment.homography,
        leftKeyPoints).unzip
    }

    println("Number of KeyPoints: %s".format(leftKeyPoints.size))

    val (leftDescriptors, rightDescriptors) = {
      val leftDescriptors = experiment.extractor.extract(leftImage, leftKeyPoints)
      val rightDescriptors = experiment.extractor.extract(rightImage, rightKeyPoints)

      for ((Some(left), Some(right)) <- leftDescriptors.zip(rightDescriptors)) yield (left, right)
    } unzip

    println("Number of surviving KeyPoints: %s".format(leftDescriptors.size))

    val dmatches = experiment.matcher.doMatch(true, leftDescriptors, rightDescriptors)

    val results = WideBaselineExperimentResults(experiment, dmatches)
    results.save
    results
  }

  def fromExperiment(
    experiment: WideBaselineExperiment): WideBaselineExperimentResults = {
    if (experiment.alreadyRun) {
      val Some(file) = experiment.existingResultsFile
      println("Reading %s".format(file))
      IO.fromJSONFileAbstract[WideBaselineExperimentResults](ExperimentIO.formats, file)
    } else runExperiment(experiment)
  }
}

