package nebula

import java.io.File
import xml._

import com.googlecode.javacv.cpp.opencv_features2d._

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

case class CorrespondenceExperimentResults[T <: AnyRef, D, E <: AnyRef, M <: AnyRef](
  experiment: CorrespondenceExperiment[T, D, E, M],
  dmatches: Seq[DMatch])(
  implicit detectorLike: DetectorLike[T],
  extractorLike: ExtractorLike[E, D],
  matcherLike: MatcherLike[M, D]) {
  // def save(implicit manifest: Manifest[CorrespondenceExperimentResults[T, D, E, M]]) {
  //   println("Writing to %s".format(experiment.path))
  //   IO.toJSONFileAbstract(ExperimentIO.formats,
  // 			  this, 
  // 			  experiment.path)
  // }
}

object CorrespondenceExperimentResults {
  def runExperiment[T <: AnyRef, D, E <: AnyRef, M <: AnyRef](
    experiment: CorrespondenceExperiment[T, D, E, M])(
    implicit detectorLike: DetectorLike[T],
    extractorLike: ExtractorLike[E, D],
    matcherLike: MatcherLike[M, D]): CorrespondenceExperimentResults[T, D, E, M] = {
    println("Running %s".format(experiment))

    val detector = detectorLike(experiment.detector)
    val extractor = extractorLike.extractMany(experiment.extractor) _
    val matcher = matcherLike(experiment.matcher)

    val leftImage = experiment.leftImage
    val rightImage = experiment.rightImage

    val (leftKeyPoints, rightKeyPoints) = {
      val leftKeyPoints =  detector(leftImage)
      Util.pruneKeyPoints(leftImage,
                          rightImage,
                          experiment.homography,
                          leftKeyPoints).unzip
    }

    println("Number of KeyPoints: %s".format(leftKeyPoints.size))

    val (leftDescriptors, rightDescriptors) = {
      val leftDescriptors = extractor(leftImage, leftKeyPoints)
      val rightDescriptors = extractor(rightImage, rightKeyPoints)

      for ((Some(left), Some(right)) <- leftDescriptors.zip(rightDescriptors)) yield (left, right)
    } unzip

    println("Number of surviving KeyPoints: %s".format(leftDescriptors.size))

    val dmatches = matcher(true, leftDescriptors, rightDescriptors)

    val results = CorrespondenceExperimentResults(experiment, dmatches)
    // TODO
//    results.save
    results
  }

  def runAbstractExperiment(experiment: CorrespondenceExperiment[_, _, _, _]): CorrespondenceExperimentResults[_, _, _, _] = {
    import shapeless.Typeable._

    experiment match {
      case resolved: CorrespondenceExperiment[FASTDetector, SortDescriptor, SortExtractor, L0Matcher] if experiment.cast[CorrespondenceExperiment[FASTDetector, SortDescriptor, SortExtractor, L0Matcher]].isDefined => runExperiment(resolved)
      case _ => sys.error("TODO")
    }
  }

  // def fromExperiment(experiment: CorrespondenceExperiment): CorrespondenceExperimentResults = {
  //   if (experiment.alreadyRun) {
  //     println("Loading completed experiment: %s".format(experiment))
  //     IO.fromJSONFileAbstract[CorrespondenceExperimentResults](
  // 	ExperimentIO.formats,
  //       experiment.existingResultsFile.get)
  //   } else {
  //     runExperiment(experiment)
  //   }
  // }
}

