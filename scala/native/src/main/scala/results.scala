package nebula

import java.io.File
import xml._

import com.googlecode.javacv.cpp.opencv_features2d._

case class PredictionAndTruth(val prediction: Double, val truth: Boolean)

case class ResultsData(val predictionsAndTruths: List[PredictionAndTruth])

object ResultsData { 
  def sorted(predictions: List[Double], truths: List[Boolean]): ResultsData = {
    val sortedPairs = predictions.zip(truths).sortBy(_._1)
    val predictionsAndTruths = 
      sortedPairs.map(e => PredictionAndTruth(e._1, e._2))
    ResultsData(predictionsAndTruths)
  }
}

case class CorrespondenceExperimentResults(experiment: CorrespondenceExperiment,
					   dmatches: List[DMatch]) {
  def save {
    println("Writing to %s".format(experiment.path))
    IO.toJSONFileAbstract(ExperimentIO.formats,
			  this, 
			  experiment.path)
  }
}

object CorrespondenceExperimentResults {
  def runExperiment(experiment: CorrespondenceExperiment): CorrespondenceExperimentResults = {
    println("Running %s".format(experiment))

    val leftImage = experiment.leftImage
    val rightImage = experiment.rightImage

    val (leftKeyPoints, rightKeyPoints) = {
      val leftKeyPoints = experiment.detector(leftImage)
      Util.pruneKeyPoints(leftImage,
                          rightImage,
                          experiment.homography,
                          leftKeyPoints).unzip
    }

    println("Number of KeyPoints: %s".format(leftKeyPoints.size))

    val (leftDescriptors, rightDescriptors) = {
      val leftDescriptors = experiment.extractor(leftImage, leftKeyPoints)
      val rightDescriptors = experiment.extractor(rightImage, rightKeyPoints)

      for ((Some(left), Some(right)) <- leftDescriptors.zip(rightDescriptors)) yield (left, right)
    } unzip

    println("Number of surviving KeyPoints: %s".format(leftDescriptors.size))

    val dmatches = experiment.matcher(true, leftDescriptors, rightDescriptors)

    val results = CorrespondenceExperimentResults(experiment, dmatches)
    results.save
    results
  }

  def fromExperiment(experiment: CorrespondenceExperiment): CorrespondenceExperimentResults = {
    if (experiment.alreadyRun) {
      println("Loading completed experiment: %s".format(experiment))
      IO.fromJSONFileAbstract[CorrespondenceExperimentResults](
	ExperimentIO.formats,
        experiment.existingResultsFile.get)
    } else {
      runExperiment(experiment)
    }
  }
}

