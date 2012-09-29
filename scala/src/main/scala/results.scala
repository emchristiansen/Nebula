package nebula

import java.io.File
import xml._

import org.opencv.core.Mat
import org.opencv.features2d.{ DMatch, KeyPoint }
import org.opencv.highgui.Highgui.imread

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
    import DetectorImpl._
    import ExtractorImpl._
    import MatcherImpl._

    val detector = experiment.detector match {
      case detector: FASTDetector => implicitly[DetectorLike[FASTDetector]].apply(detector)
      case detector: BRISKDetector => implicitly[DetectorLike[BRISKDetector]].apply(detector)
    }

    def runWithActions[D <: Descriptor](
      extractor: ExtractorAction[D],
      matcher: MatcherAction[D]): CorrespondenceExperimentResults = {
      println("Running %s".format(experiment))

      val leftImage = experiment.leftImage
      val rightImage = experiment.rightImage

      val (leftKeyPoints, rightKeyPoints) = {
        val leftKeyPoints = detector(leftImage)
        Util.pruneKeyPoints(
          leftImage,
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
      results.save
      results
    }

    // TODO: This explicit enumeration is really irritating, but I'm not
    // sure how to avoid something like this when using type classes, since
    // they provide only compile-time polymorphism. A better solution might
    // use inheritance and dynamic dispatch (sigh), though type erasure might
    // pose a problem with that approach.
    (experiment.extractor, experiment.matcher) match {
      case (extractor: RawExtractor, matcher: L0Matcher) => {
        val extractorAction = implicitly[ExtractorLike[RawExtractor, RawDescriptor[Int]]].apply(extractor)
        val matcherAction = implicitly[MatcherLike[L0Matcher, RawDescriptor[Int]]].apply(matcher)
        runWithActions(extractorAction, matcherAction)
      }

      case (extractor: RawExtractor, matcher: L1Matcher) => {
        val extractorAction = implicitly[ExtractorLike[RawExtractor, RawDescriptor[Int]]].apply(extractor)
        val matcherAction = implicitly[MatcherLike[L1Matcher, RawDescriptor[Int]]].apply(matcher)
        runWithActions(extractorAction, matcherAction)
      }

      case (extractor: RawExtractor, matcher: L2Matcher) => {
        val extractorAction = implicitly[ExtractorLike[RawExtractor, RawDescriptor[Int]]].apply(extractor)
        val matcherAction = implicitly[MatcherLike[L2Matcher, RawDescriptor[Int]]].apply(matcher)
        runWithActions(extractorAction, matcherAction)
      }

      case (extractor: SortExtractor, matcher: L0Matcher) => {
        val extractorAction = implicitly[ExtractorLike[SortExtractor, SortDescriptor]].apply(extractor)
        val matcherAction = implicitly[MatcherLike[L0Matcher, SortDescriptor]].apply(matcher)
        runWithActions(extractorAction, matcherAction)
      }

      case (extractor: SortExtractor, matcher: L1Matcher) => {
        val extractorAction = implicitly[ExtractorLike[SortExtractor, SortDescriptor]].apply(extractor)
        val matcherAction = implicitly[MatcherLike[L1Matcher, SortDescriptor]].apply(matcher)
        runWithActions(extractorAction, matcherAction)
      }

      case (extractor: SortExtractor, matcher: L2Matcher) => {
        val extractorAction = implicitly[ExtractorLike[SortExtractor, SortDescriptor]].apply(extractor)
        val matcherAction = implicitly[MatcherLike[L2Matcher, SortDescriptor]].apply(matcher)
        runWithActions(extractorAction, matcherAction)
      }

      case (extractor: SortExtractor, matcher: KendallTauMatcher) => {
        val extractorAction = implicitly[ExtractorLike[SortExtractor, SortDescriptor]].apply(extractor)
        val matcherAction = implicitly[MatcherLike[KendallTauMatcher, SortDescriptor]].apply(matcher)
        runWithActions(extractorAction, matcherAction)
      }

      case (extractor: SortExtractor, matcher: CayleyMatcher) => {
        val extractorAction = implicitly[ExtractorLike[SortExtractor, SortDescriptor]].apply(extractor)
        val matcherAction = implicitly[MatcherLike[CayleyMatcher, SortDescriptor]].apply(matcher)
        runWithActions(extractorAction, matcherAction)
      }

      case (extractor: SortExtractor, matcher: CayleyRotate4Matcher) => {
        val extractorAction = implicitly[ExtractorLike[SortExtractor, SortDescriptor]].apply(extractor)
        val matcherAction = implicitly[MatcherLike[CayleyRotate4Matcher, SortDescriptor]].apply(matcher)
        runWithActions(extractorAction, matcherAction)
      }

      case (extractor: BRISKExtractor, matcher: L0Matcher) => {
        val extractorAction = implicitly[ExtractorLike[BRISKExtractor, RawDescriptor[Boolean]]].apply(extractor)
        val matcherAction = implicitly[MatcherLike[L0Matcher, RawDescriptor[Boolean]]].apply(matcher)
        runWithActions(extractorAction, matcherAction)
      }

      case (extractor: FREAKExtractor, matcher: L0Matcher) => {
        val extractorAction = implicitly[ExtractorLike[FREAKExtractor, RawDescriptor[Boolean]]].apply(extractor)
        val matcherAction = implicitly[MatcherLike[L0Matcher, RawDescriptor[Boolean]]].apply(matcher)
        runWithActions(extractorAction, matcherAction)
      }

      case (extractor: ELUCIDExtractor, matcher: L0Matcher) => {
        val extractorAction = implicitly[ExtractorLike[ELUCIDExtractor, SortDescriptor]].apply(extractor)
        val matcherAction = implicitly[MatcherLike[L0Matcher, SortDescriptor]].apply(matcher)
        runWithActions(extractorAction, matcherAction)
      }

      case (extractor: ELUCIDExtractor, matcher: L1Matcher) => {
        val extractorAction = implicitly[ExtractorLike[ELUCIDExtractor, SortDescriptor]].apply(extractor)
        val matcherAction = implicitly[MatcherLike[L1Matcher, SortDescriptor]].apply(matcher)
        runWithActions(extractorAction, matcherAction)
      }

      case (extractor: ELUCIDExtractor, matcher: CayleyMatcher) => {
        val extractorAction = implicitly[ExtractorLike[ELUCIDExtractor, SortDescriptor]].apply(extractor)
        val matcherAction = implicitly[MatcherLike[CayleyMatcher, SortDescriptor]].apply(matcher)
        runWithActions(extractorAction, matcherAction)
      }

      case (extractor: ELUCIDExtractor, matcher: KendallTauMatcher) => {
        val extractorAction = implicitly[ExtractorLike[ELUCIDExtractor, SortDescriptor]].apply(extractor)
        val matcherAction = implicitly[MatcherLike[KendallTauMatcher, SortDescriptor]].apply(matcher)
        runWithActions(extractorAction, matcherAction)
      }

      case (extractor: RawExtractor, matcher: RobustCayleyMatcher) => {
        val extractorAction = implicitly[ExtractorLike[RawExtractor, RawDescriptor[Int]]].apply(extractor)
        val matcherAction = implicitly[MatcherLike[RobustCayleyMatcher, RawDescriptor[Int]]].apply(matcher)
        runWithActions(extractorAction, matcherAction)
      }

      case (extractor: RawExtractor, matcher: GeneralizedL0Matcher) => {
        val extractorAction = implicitly[ExtractorLike[RawExtractor, RawDescriptor[Int]]].apply(extractor)
        val matcherAction = implicitly[MatcherLike[GeneralizedL0Matcher, RawDescriptor[Int]]].apply(matcher)
        runWithActions(extractorAction, matcherAction)
      }

      case _ => sys.error("You must manually add the experiment to this list")
    }
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

