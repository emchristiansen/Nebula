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

case class CorrespondenceExperimentResults(
  experiment: CorrespondenceExperiment,
  dmatches: Seq[DMatch]) {
  def save {
    println("Writing to %s".format(experiment.path))
    IO.toJSONFileAbstract(
      ExperimentIO.formats,
      this,
      experiment.path)
  }
}

object CorrespondenceExperimentResults {
  def runExperiment(
    experiment: CorrespondenceExperiment): CorrespondenceExperimentResults = {


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

      // Use the explicit descriptor type to cast the extractor and matcher to their
      // true dynamic types.
      type DescriptorType = experiment.extractor.DescriptorType
      val extractor = experiment.extractor.asInstanceOf[ExtractorParameterized[DescriptorType]]
      val matcher = experiment.matcher.asInstanceOf[MatcherParameterized[DescriptorType]]
      
      val (leftDescriptors, rightDescriptors) = {
        val leftDescriptors = extractor.extract(leftImage, leftKeyPoints)
        val rightDescriptors = extractor.extract(rightImage, rightKeyPoints)

        for ((Some(left), Some(right)) <- leftDescriptors.zip(rightDescriptors)) yield (left, right)
      } unzip

      println("Number of surviving KeyPoints: %s".format(leftDescriptors.size))

      val dmatches = matcher.doMatch(true, leftDescriptors, rightDescriptors)

      val results = CorrespondenceExperimentResults(experiment, dmatches)
      results.save
      results
  }

  def fromExperiment(
    experiment: CorrespondenceExperiment): CorrespondenceExperimentResults = {
    if (experiment.alreadyRun) {
      val Some(file) = experiment.existingResultsFile
      println("Reading %s".format(file))
      IO.fromJSONFileAbstract[CorrespondenceExperimentResults](ExperimentIO.formats, file)
    } else runExperiment(experiment)
  }
}

