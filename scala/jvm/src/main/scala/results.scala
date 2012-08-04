package nebula

import java.io.File
import xml._

case class PredictionAndTruth(val prediction: Double, val truth: Boolean) {
  def toXML: Node = {
    <predictionAndTruth>
      <prediction>{prediction}</prediction>
      <truth>{truth}</truth>
    </predictionAndTruth>
  }
}

object PredictionAndTruth {
  def fromXML(node: Node): PredictionAndTruth = {
    def text = XMLUtil.text(node) _

    PredictionAndTruth(
      text("prediction").toDouble,
      text("truth").toBoolean
    )
  }
}

case class ResultsData(val predictionsAndTruths: List[PredictionAndTruth]) {
  def toXML: Node = {
    val list = predictionsAndTruths.map(_.toXML)
    <resultsData>{list}</resultsData>
  }
}

object ResultsData { 
  def sorted(predictions: List[Double], truths: List[Boolean]): ResultsData = { 
    val predictionsAndTruths = predictions.zip(truths).sortBy(_._1).map(e => PredictionAndTruth(e._1, e._2))
    ResultsData(predictionsAndTruths)
  }

  def fromXML(node: Node): ResultsData = {
    val (predictions, truths) = (for (xmlPT <- (node \ "predictionAndTruth")) yield {
      val prediction = (xmlPT \ "prediction").head.text.toDouble
      val truth = (xmlPT \ "truth").head.text.toBoolean
      (prediction, truth)
    }).unzip

    ResultsData.sorted(predictions.toList, truths.toList)
  }
}

case class ExperimentResults(val experiment: Experiment, val resultsDataList: List[ResultsData]) {
  def toXML: Node = {
    val list = resultsDataList.map(_.toXML)
    <experimentResults>
      {experiment.toXML}
      {list}
    </experimentResults>
  }

  // TODO: rename to toFile
  def write { 
    println("writing: %s".format(experiment.outPath))
    XMLUtil.save(experiment.outPath, toXML)
  }
}

object ExperimentResults {
  def fromXML(node: Node): ExperimentResults = {
    val experiment = Experiment.fromXML((node \ "experiment").head)
    val resultsDataList = (node \ "resultsData").map(ResultsData.fromXML).toList
    ExperimentResults(experiment, resultsDataList)
  }

  def fromTxtFile(path: String): ExperimentResults = {
    println(path)
    val contents = io.Source.fromFile(path).mkString.split("\n").filter(_.size > 0).map(_.split(" "))

    def checker(index: Int, tag: String): List[String] = {
      val line = contents(index)
      assert(line(0) == tag)
      line.toList.tail
    }
    
    val List(time) = checker(0, "time:")
    val List(roi) = checker(1, "roi:")
    val List(distance) = checker(2, "distance:")
    val List(poseLeft, poseRight) = checker(3, "pose:")
    val List(illuminationLeft, illuminationRight) = checker(4, "illumination:")
    val List(blurLeft, blurRight) = checker(5, "blur:")
    val List(noiseLeft, noiseRight) = checker(6, "noise:")
    val List(jpegLeft, jpegRight) = checker(7, "jpeg:")
    val List(misalignmentLeft, misalignmentRight) = checker(8, "misalignment:")
    def newName(oldBackgroundValue: String) = oldBackgroundValue match { 
      case "true" => "synthetic"
      case "false" => "blank"
    }
    val backgroundLeft = newName(checker(9, "background:")(0))
    val backgroundRight = newName(checker(9, "background:")(1))

    val leftCondition = Condition(poseLeft, illuminationLeft, blurLeft, noiseLeft, jpegLeft, misalignmentLeft, backgroundLeft)
    val rightCondition = Condition(poseRight, illuminationRight, blurRight, noiseRight, jpegRight, misalignmentRight, backgroundRight)
    val experiment = Experiment(roi, distance, leftCondition, rightCondition)

    def processFoldBlock(index: Int): ResultsData = {
      val predictions = contents(index + 1).map(_.toDouble).toList
      val truths = contents(index + 2).map(_.toBoolean).toList
      ResultsData(predictions.zip(truths).map({case (p, t) => PredictionAndTruth(p, t)}))
    }

    val indices = contents.zipWithIndex.filter({case (l, i) => l(0) == "#" && l(1) == "Fold"}).map(_._2).toList
    assert(indices.size == 5);
    val resultsDataList: List[ResultsData] = for (i <- indices) yield {processFoldBlock(i)}
    
    ExperimentResults(experiment, resultsDataList)
  }

  def fromCompletedExperiment(experiment: Experiment): ExperimentResults = {
    assert(experiment.alreadyRun)
    fromXML(XML.loadFile(experiment.existingResultsPath))
  }

  def fromFile(path: String): ExperimentResults = fromXML(XML.loadFile(path))
}
