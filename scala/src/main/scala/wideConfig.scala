package nebula

import javax.imageio.ImageIO
import org.opencv.features2d.DMatch

///////////////////////////////////////////////////////////

case class WideBaselineExperiment(
  imageClass: String,
  otherImage: Int,
  detector: Detector,
  extractor: Extractor,
  matcher: Matcher)

object WideBaselineExperiment {
  implicit def implicitHasGroundTruth(self: WideBaselineExperiment): HasGroundTruth[Homography] =
    new HasGroundTruth[Homography] {
      override def groundTruth = {
        val homographyFile = Global.run[RuntimeConfig].projectChildPath("data/oxfordImages/%s/homographies/H1to%sp".format(self.imageClass, self.otherImage))
        Homography.fromFile(homographyFile)
      }
    }

  implicit def implicitExperiment(self: WideBaselineExperiment): Experiment with HasOriginal =
    new Experiment with HasOriginal {
      override def name = "WideBaselineExperiment"
      override def parameters = Seq(
        ("IC", self.imageClass),
        ("OI", self.otherImage.toString),
        ("D", Util.abbreviate(self.detector)),
        ("E", Util.abbreviate(self.extractor)),
        ("M", Util.abbreviate(self.matcher)))
      override def original = self
    }

  implicit def implicitImagePairLike(self: WideBaselineExperiment): ImagePairLike =
    new ImagePairLike {
      override def leftImage = {
        val file = Global.run[RuntimeConfig].projectChildPath("data/oxfordImages/%s/images/img1.bmp".format(self.imageClass))
        ImageIO.read(file)
      }
      override def rightImage = {
        val file = Global.run[RuntimeConfig].projectChildPath("data/oxfordImages/%s/images/img%s.bmp".format(self.imageClass, self.otherImage))
        ImageIO.read(file)
      }
    }
}

///////////////////////////////////////////////////////////

case class WideBaselineExperimentResults(
  val experiment: WideBaselineExperiment,
  val dmatches: Seq[DMatch])

object WideBaselineExperimentResults {
  def apply(
    experiment: WideBaselineExperiment): WideBaselineExperimentResults = {
    // TODO: Code is duplicated
    val noResults = WideBaselineExperimentResults(experiment, null)
    if (noResults.alreadyRun && Global.run[RuntimeConfig].skipCompletedExperiments) {
      val Some(file) = noResults.existingResultsFile
      println("Reading %s".format(file))
      IO.fromJSONFileAbstract[WideBaselineExperimentResults](ExperimentIO.formats, file)
    } else run(experiment)
  }

  private def run(self: WideBaselineExperiment): WideBaselineExperimentResults = {
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
      override def save = {
        println("Writing to %s".format(self.path))
        IO.toJSONFileAbstract(
          ExperimentIO.formats,
          WideBaselineExperimentResults.this,
          self.path)
      }
    }
}