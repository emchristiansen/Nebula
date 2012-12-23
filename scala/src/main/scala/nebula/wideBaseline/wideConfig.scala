package nebula.wideBaseline

import javax.imageio.ImageIO
import org.opencv.features2d.DMatch
import nebula.util.Util
import nebula.util.Homography
import nebula.HasGroundTruth
import nebula.RuntimeConfig
import nebula.HasImagePair
import nebula.Experiment
import nebula.Detector
import nebula.Extractor
import nebula.ExperimentResults
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
//import net.liftweb.json.JsonAST.JField
//import net.liftweb.json.JsonAST.JObject

import DetectorJsonProtocol._
import ExtractorJsonProtocol._
import MatcherJsonProtocol._
import ExperimentJsonProtocol._
import ExperimentResultsJsonProtocol._

import spray.json._
import nebula.summary._

///////////////////////////////////////////////////////////

case class WideBaselineExperiment(
  imageClass: String,
  otherImage: Int,
  detector: Detector,
  extractor: Extractor,
  matcher: Matcher)

object WideBaselineExperiment {
  implicit def implicitHasGroundTruth(
    self: WideBaselineExperiment)(
      implicit runtime: RuntimeConfig): HasGroundTruth[Homography] =
    new HasGroundTruth[Homography] {
      override def groundTruth = {
        val homographyFile = runtime.projectChildPath(
          "data/oxfordImages/%s/homographies/H1to%sp".format(
            self.imageClass,
            self.otherImage))
        Homography.fromFile(homographyFile)
      }
    }

  implicit def implicitExperiment(self: WideBaselineExperiment): Experiment =
    new Experiment {
      override def name = "WideBaselineExperiment"
      override def parameters = Seq(
        ("IC", self.imageClass.toString),
        ("OI", self.otherImage.toString),
        ("D", JSONUtil.abbreviate(self.detector)),
        ("E", JSONUtil.abbreviate(self.extractor)),
        ("M", JSONUtil.abbreviate(self.matcher)))
      override def original = self

      override def getResults(implicit runtime: RuntimeConfig) =
        WideBaselineExperimentResults(self)
    }

  implicit def implicitImagePairLike(
    self: WideBaselineExperiment)(
      implicit runtime: RuntimeConfig): HasImagePair =
    new HasImagePair {
      override def leftImage = {
        val file = runtime.projectChildPath(
          "data/oxfordImages/%s/images/img1.bmp".format(
            self.imageClass))
        ImageIO.read(file)
      }
      override def rightImage = {
        val file = runtime.projectChildPath(
          "data/oxfordImages/%s/images/img%s.bmp".format(
            self.imageClass,
            self.otherImage))
        ImageIO.read(file)
      }
    }
}

///////////////////////////////////////////////////////////

case class WideBaselineExperimentResults(
  experiment: WideBaselineExperiment,
  dmatches: Seq[DMatch])

object WideBaselineExperimentResults {
  def apply(
    experiment: WideBaselineExperiment)(
      implicit runtime: RuntimeConfig): WideBaselineExperimentResults = {
    // TODO: Code is duplicated
    val noResults = WideBaselineExperimentResults(experiment, null)
    if (noResults.alreadyRun && runtime.skipCompletedExperiments) {
      val Some(file) = noResults.existingResultsFile
      println("Reading %s".format(file))
      val jsonString = org.apache.commons.io.FileUtils.readFileToString(file)
      jsonString.asJson.convertTo[WideBaselineExperimentResults]
    } else run(experiment)
  }

  private def run(
    self: WideBaselineExperiment)(
      implicit runtime: RuntimeConfig): WideBaselineExperimentResults = {
    println("Running %s".format(self))

    val leftImage = self.leftImage
    val rightImage = self.rightImage

    val (leftKeyPoints, rightKeyPoints) = {
      val leftKeyPoints = self.detector.detect(leftImage)
      Util.pruneKeyPoints(
        leftImage,
        rightImage,
        self.groundTruth,
        leftKeyPoints).unzip
    }

    println("Number of KeyPoints: %s".format(leftKeyPoints.size))

    val (leftDescriptors, rightDescriptors) = {
      val leftDescriptors = self.extractor.extract(leftImage, leftKeyPoints)
      val rightDescriptors = self.extractor.extract(rightImage, rightKeyPoints)

      for ((Some(left), Some(right)) <- leftDescriptors.zip(rightDescriptors)) yield (left, right)
    } unzip

    println("Number of surviving KeyPoints: %s".format(leftDescriptors.size))

    val dmatches = self.matcher.doMatch(true, leftDescriptors, rightDescriptors)

    val results = WideBaselineExperimentResults(self, dmatches)
    results.save
    results
  }

  implicit def implicitExperimentResults(self: WideBaselineExperimentResults): ExperimentResults =
    new ExperimentResults {
      override def experiment = self.experiment
      override def save(implicit runtime: RuntimeConfig) = {
        println("Writing to %s".format(self.path))
        val json = self.toJson
        org.apache.commons.io.FileUtils.writeStringToFile(self.path, json.prettyPrint)
      }
      override def original = self

      // TODO: Pull into another record.      
      override def toSummary(implicit runtime: RuntimeConfig) = new ExperimentSummary {
        def original = self
        def results = self
        def summaryNumbers = Map(
          "recognitionRate" -> Memoize(() => SummaryUtil.recognitionRate(self.dmatches)))
        def summaryImages = Map(
          "histogram" -> Memoize(() => Histogram(self, "").render))
      }
    }
}

///////////////////////////////////////////////////////////

//object WideBaselineExperimentSummary {
//  implicit def implicitWideBaselineExperimentResults(
//    self: WideBaselineExperimentResults)(
//      implicit runtime: RuntimeConfig) = new {
//    def toSummary = new ExperimentSummary {
//      def original = self
//      def results = self
//      def summaryNumbers = Map(
//        "recognitionRate" -> Memoize(() => SummaryUtil.recognitionRate(self.dmatches)))
//      def summaryImages = Map(
//        "histogram" -> Memoize(() => Histogram(self, "").render))
//    }
//  }
//}



