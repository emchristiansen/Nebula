package nebula.wideBaseline

import java.io.File

import org.opencv.features2d.DMatch

import WideBaselineJsonProtocol.wideBaselineExperiment
import WideBaselineJsonProtocol.wideBaselineExperimentResults
import javax.imageio.ImageIO
import nebula.ExperimentRunner
import nebula.Extractor
import nebula.HasGroundTruth
import nebula.HasImagePair
import nebula.Matcher
import nebula.PairDetector
import nebula.RuntimeConfig
import nebula.StorageInfo
import nebula.summary.ExperimentSummary
import nebula.summary.Histogram
import nebula.summary.SummaryUtil
import nebula.util.Homography
import nebula.util.JSONUtil
import nebula.util.Logging
import nebula.util.Memoize
import spray.json.JsonFormat
import spray.json.pimpAny
import spray.json.pimpString

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

  implicit class ImplicitExperimentRunnerWithRuntime[D, E, M, F](
    self: WideBaselineExperiment[D, E, M, F])(
      implicit runtimeConfig: RuntimeConfig,
      evPairDetector: D => PairDetector,
      evExtractor: E => Extractor[F],
      evMatcher: M => Matcher[F]) extends ExperimentRunner[WideBaselineExperimentResults[D, E, M, F]] {
    override def run = WideBaselineExperimentResults(self)
  }

  implicit def implicitExperimentRunner[D, E, M, F](
    self: WideBaselineExperiment[D, E, M, F])(
      implicit evPairDetector: D => PairDetector,
      evExtractor: E => Extractor[F],
      evMatcher: M => Matcher[F]): RuntimeConfig => ExperimentRunner[WideBaselineExperimentResults[D, E, M, F]] =
    runtime => {
      implicit val implicitRuntime = runtime
      self
    }

  implicit class ImplicitStorageInfoWithRuntime[D, E, M, F](
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

  implicit def implicitStorageInfo[D, E, M, F](
    self: WideBaselineExperiment[D, E, M, F])(
      implicit evPairDetector: D => PairDetector,
      evExtractor: E => Extractor[F],
      evMatcher: M => Matcher[F],
      evDJson: JsonFormat[D],
      evEJson: JsonFormat[E],
      evMJson: JsonFormat[M]): RuntimeConfig => StorageInfo[WideBaselineExperimentResults[D, E, M, F]] =
    runtime => {
      implicit val implicitRuntime = runtime
      self
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

  implicit class ImplicitExperimentSummaryWithConfig[D, E, M, F](
    self: WideBaselineExperimentResults[D, E, M, F])(
      implicit runtimeConfig: RuntimeConfig) extends ExperimentSummary {
    override def summaryNumbers = Map(
      "recognitionRate" -> Memoize(() => SummaryUtil.recognitionRate(self.dmatches)))
    override def summaryImages = Map(
      "histogram" -> Memoize(() => Histogram(self, "").render))
  }
  
  implicit def implicitExperimentSummary[D, E, M, F](
    self: WideBaselineExperimentResults[D, E, M, F]): RuntimeConfig => ExperimentSummary = runtime => {
      implicit val implicitRuntime = runtime
      self
    }
}





