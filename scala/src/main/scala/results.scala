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
  def runExperiment(experiment: CorrespondenceExperiment) = {
    val explicit = {
      val explicit = CorrespondenceExperimentParameterized(experiment)
      explicit
//      val matched = experiment match {
//        case x: CorrespondenceExperimentParameterized[_, _] => x
//      }
      
//      type ExtractorDescriptorType = explicit.extractor.DescriptorType
//      type MatcherDescriptorType = explicit.matcher.DescriptorType
//
//      matched.asInstanceOf[CorrespondenceExperimentParameterized[MatcherDescriptorType, ExtractorDescriptorType]]
    }

    runExperimentParameterized(explicit)//(explicit.descriptorConverter)
  }

  def runExperimentParameterized[MD, ED <% MD](
    experiment: CorrespondenceExperimentParameterized[MD, ED]): CorrespondenceExperimentResults = {

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

    //    def explicit[D](extractor: ExtractorParameterized[D], matcher: MatcherParameterized[D]) = {
    val (leftDescriptors, rightDescriptors) = {
      val leftDescriptors = experiment.extractor.extract(leftImage, leftKeyPoints)
      val rightDescriptors = experiment.extractor.extract(rightImage, rightKeyPoints)

      for ((Some(left), Some(right)) <- leftDescriptors.zip(rightDescriptors)) yield (left, right)
    } unzip

    println("Number of surviving KeyPoints: %s".format(leftDescriptors.size))

    val edToMD = implicitly[ED => MD]

    val dmatches = experiment.matcher.doMatch(true, leftDescriptors.map(edToMD), rightDescriptors.map(edToMD))

    val results = CorrespondenceExperimentResults(CorrespondenceExperiment(experiment), dmatches)
    results.save
    results
    //    }

    //    // Use the explicit descriptor type to cast the extractor and matcher to their
    //    // true dynamic types.
    //    type DescriptorType = experiment.extractor.DescriptorType
    //    val extractor = experiment.extractor.asInstanceOf[ExtractorParameterized[DescriptorType]]
    //    val matcher = experiment.matcher.asInstanceOf[MatcherParameterized[DescriptorType]]    

    //    // TODO
    //    (experiment.extractor, experiment.matcher) match {
    //      case (e: ExtractorParameterized[IndexedSeq[Int]], 
    //          m: MatcherParameterized[IndexedSeq[Int]]) => explicit(e, m)
    //      case (e: ExtractorParameterized[SortDescriptor],
    //          m: MatcherParameterized[SortDescriptor]) => explicit(e, m)
    //      case _ => sys.error("descriptor type not recognized")
    //    }

    //    explicit(extractor, matcher)
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

