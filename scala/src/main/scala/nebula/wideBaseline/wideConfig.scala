package nebula.wideBaseline

import javax.imageio.ImageIO
import org.opencv.features2d.DMatch
import nebula.util.Util
import nebula.util.Homography
import nebula.HasGroundTruth
import nebula.RuntimeConfig
import nebula.HasImagePair

import nebula.Detector
import nebula.Extractor

import nebula.util.IO
import nebula.Matcher
import nebula.util.ExperimentIO
import nebula.graveyard._
import nebula.mpie._
import nebula.summary._
import nebula.smallBaseline._
import nebula.util._
import nebula.util.imageProcessing._
import nebula.wideBaseline._
import nebula._

import DetectorJsonProtocol._
import ExtractorJsonProtocol._
import MatcherJsonProtocol._

import PairDetectorJsonProtocol._

import spray.json._
import nebula.summary._

import java.io.File

import nebula.util.JSONUtil._

///////////////////////////////////////////////////////////

case class WideBaselineExperiment[D, E, M, F](
  imageClass: String,
  otherImage: Int,
  detector: D,
  extractor: E,
  matcher: M)(
    implicit evPairDetector: D => PairDetector,
    evExtractor: E => Extractor[F],
    evMatcher: M => Matcher[F])

object WideBaselineExperiment {
  import WideBaselineJsonProtocol._

  implicit def implicitHasGroundTruth(
    self: WideBaselineExperiment[_, _, _, _])(
      implicit runtime: RuntimeConfig): HasGroundTruth[Homography] =
    new HasGroundTruth[Homography] {
      override def groundTruth = {
        val homographyFile = runtime.projectChildPath(
          s"data/oxfordImages/${self.imageClass}/homographies/H1to${self.otherImage}p")
        Homography.fromFile(homographyFile)
      }
    }

  //  implicit def implicitExperiment[D, E, M, F](self: WideBaselineExperiment[D, E, M, F])(
  //    implicit runtime: RuntimeConfig,
  //      evPairDetector: D => PairDetector,
  //      evExtractor: E => Extractor[F],
  //      evMatcher: M => Matcher[F],
  //      evDJson: JsonFormat[D],
  //      evEJson: JsonFormat[E],
  //      evMJson: JsonFormat[M]): Experiment =
  //    new Experiment {
  //      override def getResults(implicit runtime: RuntimeConfig) =
  //        WideBaselineExperimentResults(self)
  //    }

  implicit class ImplicitExperimentRunner[D, E, M, F](
    self: WideBaselineExperiment[D, E, M, F])(
      implicit runtimeConfig: RuntimeConfig,
      evPairDetector: D => PairDetector,
      evExtractor: E => Extractor[F],
      evMatcher: M => Matcher[F]) extends ExperimentRunner[WideBaselineExperimentResults[D, E, M, F]] {
    override def run = WideBaselineExperimentResults(self)
  }

  implicit class ImplicitStorageInfo[D, E, M, F](
    self: WideBaselineExperiment[D, E, M, F])(
      implicit runtime: RuntimeConfig,
      evPairDetector: D => PairDetector,
      evExtractor: E => Extractor[F],
      evMatcher: M => Matcher[F],
      evDJson: JsonFormat[D],
      evEJson: JsonFormat[E],
      evMJson: JsonFormat[M]) extends StorageInfo[WideBaselineExperimentResults[D, E, M, F]] {
    override def currentPath: File = new File(outDirectory, filename)

    override def mostRecentPath: Option[File] = existingResultsFiles.headOption

    override def save = results => {
      println(s"Saving to ${currentPath}")
      val json = results.toJson
      org.apache.commons.io.FileUtils.writeStringToFile(currentPath, json.prettyPrint)
    }

    override def load = mostRecentPath map { file =>
      println(s"Loading ${file}")
      val jsonString = org.apache.commons.io.FileUtils.readFileToString(file)
      jsonString.asJson.convertTo[WideBaselineExperimentResults[D, E, M, F]]
    }

    override def name = unixEpoch + "_" + nameNoTime

    ///////////////////////////////////////////////////////

    val unixEpoch = System.currentTimeMillis / 1000L

    def nameNoTime: String = {
      val fullString = JSONUtil.flattenJson(self.toJson)
      // Unfortunately, this string is too long to be part of a filename.
      fullString.take(100) + "_" + fullString.hashCode
    }

    def filenameNoTime: String = nameNoTime + ".json"

    def filename: String = unixEpoch + "_" + filenameNoTime

    def outDirectory: File = runtime.projectChildPath(
      "results/experiment_data/")

    def existingResultsFiles: Seq[File] = {
      val allPaths = outDirectory.list.toList.map(path => outDirectory + "/" + path.toString)
      val matchingPaths = allPaths.filter(_.contains(filenameNoTime))
      matchingPaths.sortBy(identity).reverse.map(path => new File(path))
    }
  }

  implicit class ImplicitImagePairLike(
    self: WideBaselineExperiment[_, _, _, _])(
      implicit runtime: RuntimeConfig) extends HasImagePair {
    override def leftImage = {
      val file = runtime.projectChildPath(
        s"data/oxfordImages/${self.imageClass}/images/img1.bmp")
      ImageIO.read(file)
    }
    override def rightImage = {
      val file = runtime.projectChildPath(
        s"data/oxfordImages/${self.imageClass}/images/img${self.otherImage}.bmp")
      ImageIO.read(file)
    }
  }
}

///////////////////////////////////////////////////////////

case class WideBaselineExperimentResults[D, E, M, F](
  experiment: WideBaselineExperiment[D, E, M, F],
  dmatches: Seq[DMatch])

object WideBaselineExperimentResults extends Logging {
  import WideBaselineJsonProtocol._

  def apply[D, E, M, F](
    experiment: WideBaselineExperiment[D, E, M, F])(
      implicit runtimeConfig: RuntimeConfig,
      evPairDetector: D => PairDetector,
      evExtractor: E => Extractor[F],
      evMatcher: M => Matcher[F]): WideBaselineExperimentResults[D, E, M, F] = {
    run(experiment)
  }

  private def run[D, E, M, F](
    self: WideBaselineExperiment[D, E, M, F])(
      implicit runtimeConfig: RuntimeConfig,
      evPairDetector: D => PairDetector,
      evExtractor: E => Extractor[F],
      evMatcher: M => Matcher[F]): WideBaselineExperimentResults[D, E, M, F] = {
    println(s"Running ${self}")

    val leftImage = self.leftImage
    val rightImage = self.rightImage

    val (leftKeyPoints, rightKeyPoints) = self.detector.detectPair(
      self.groundTruth,
      leftImage,
      rightImage) unzip

    println(s"Number of KeyPoints: ${leftKeyPoints.size}")

    val (leftDescriptors, rightDescriptors) = {
      val leftDescriptors = self.extractor.extract(leftImage, leftKeyPoints)
      val rightDescriptors = self.extractor.extract(rightImage, rightKeyPoints)

      for ((Some(left), Some(right)) <- leftDescriptors.zip(rightDescriptors)) yield (left, right)
    } unzip

    println(s"Number of surviving KeyPoints: ${leftDescriptors.size}")

    val dmatches = self.matcher.doMatch(true, leftDescriptors, rightDescriptors)

    WideBaselineExperimentResults(self, dmatches)
  }

  implicit class ImplicitExperimentSummary[D, E, M, F](
    self: WideBaselineExperimentResults[D, E, M, F])(
      implicit runtimeConfig: RuntimeConfig) extends ExperimentSummary {
    override def summaryNumbers = Map(
      "recognitionRate" -> Memoize(() => SummaryUtil.recognitionRate(self.dmatches)))
    override def summaryImages = Map(
      "histogram" -> Memoize(() => Histogram(self, "").render))
  }
}

import nebula.util.DMatchJsonProtocol._

object WideBaselineJsonProtocol extends DefaultJsonProtocol {
  implicit def wideBaselineExperiment[D, E, M, F](
    implicit evPairDetector: D => PairDetector,
    evExtractor: E => Extractor[F],
    evMatcher: M => Matcher[F],
    evDJson: JsonFormat[D],
    evEJson: JsonFormat[E],
    evMJson: JsonFormat[M]): RootJsonFormat[WideBaselineExperiment[D, E, M, F]] =
    jsonFormat5(WideBaselineExperiment.apply[D, E, M, F]).addClassInfo("WideBaselineExperiment")

  implicit def wideBaselineExperimentResults[D, E, M, F](
    implicit evPairDetector: D => PairDetector,
    evExtractor: E => Extractor[F],
    evMatcher: M => Matcher[F],
    evDJson: JsonFormat[D],
    evEJson: JsonFormat[E],
    evMJson: JsonFormat[M]): RootJsonFormat[WideBaselineExperimentResults[D, E, M, F]] =
    jsonFormat2(WideBaselineExperimentResults.apply[D, E, M, F]).addClassInfo(
      "WideBaselineExperimentResults")
}



