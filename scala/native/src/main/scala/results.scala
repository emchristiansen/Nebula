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
    IO.toJSONFileAbstract(CorrespondenceExperimentConfig.formats,
			  this, 
			  experiment.path)
  }
}

