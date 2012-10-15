package nebula

import javax.imageio.ImageIO
import breeze.linalg._
import java.awt.image.BufferedImage

trait ImagePairLike {
  def leftImage: BufferedImage
  def rightImage: BufferedImage
}

case class WideBaselineExperiment(
  val imageClass: String,
  val otherImage: Int,
  val detector: Detector,
  val extractor: Extractor,
  val matcher: Matcher)

object WideBaselineExperiment {
  implicit def implicitHomography(self: WideBaselineExperiment) = new {
    def homography = {
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

  implicit def implicitImagePairLike(self: WideBaselineExperiment): ImagePairLike = {
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
}

case class SmallBaselineExperiment(
  val searchRadius: Int,
  val imageClass: String,
  val extractor: Extractor,
  val matcher: Matcher)

object SmallBaselineExperiment {
  implicit def implicitExperiment(self: SmallBaselineExperiment): Experiment =
    new Experiment {
      override def name = "SmallBaselineExperiment"
      override def parameters = Seq(
        ("SR", self.searchRadius.toString),
        ("IC", self.imageClass),
        ("E", Util.abbreviate(self.extractor)),
        ("M", Util.abbreviate(self.matcher)))
    }
}
