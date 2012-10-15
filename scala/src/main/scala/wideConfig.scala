package nebula

import javax.imageio.ImageIO

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

  implicit def implicitExperiment(self: WideBaselineExperiment): Experiment =
    new Experiment {
      override def name = "WideBaselineExperiment"
      override def parameters = Seq(
        ("IC", self.imageClass),
        ("OI", self.otherImage.toString),
        ("D", Util.abbreviate(self.detector)),
        ("E", Util.abbreviate(self.extractor)),
        ("M", Util.abbreviate(self.matcher)))
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
