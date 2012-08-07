package nebula

import java.io.File
import xml._

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

trait Results

case class ExperimentResults(experiment: Experiment, results: Results)

