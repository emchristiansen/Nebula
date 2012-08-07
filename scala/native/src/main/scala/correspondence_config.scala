package nebula

import javax.imageio.ImageIO

case class CorrespondenceExperimentConfig[D <: DescriptorTrait[_]](
  val imageClasses: List[String],
  val otherImages: List[Int],
  val detectors: List[DetectorMethod],
  val extractors: List[ExtractorMethod[D]],
  val matchers: List[MatcherMethod[D]])

case class CorrespondenceExperiment[D <: DescriptorTrait[_]](
  val imageClass: String,
  val otherImage: Int,
  val detector: DetectorMethod,
  val extractor: ExtractorMethod[D],
  val matcher: MatcherMethod[D]) extends Experiment {
  val parameterAbbreviations: List[String] = "IC OI D E M".split(" ").toList
  val parameterValues: List[String] = List(imageClass, 
					   otherImage.toString, 
					   detector.abbreviation, 
					   extractor.abbreviation, 
					   matcher.abbreviation)

  lazy val leftImageFile = Global.run.childPath("data/%s/images/img1.bmp".format(imageClass))
  def leftImage = ImageIO.read(leftImageFile)
  lazy val rightImageFile = Global.run.childPath("data/%s/images/img%s.bmp".format(imageClass, otherImage))
  def rightImage = ImageIO.read(rightImageFile)
  lazy val homographyFile = Global.run.childPath("data/%s/homographies/H1to%sp".format(imageClass, otherImage))
  def homography = Homography.fromFile(homographyFile)
}

object CorrespondenceExperiment{
  def fromConfig[D <: DescriptorTrait[_]](config: CorrespondenceExperimentConfig[D]): List[CorrespondenceExperiment[D]] = {
    for (ic <- config.imageClasses;
	 oi <- config.otherImages;
	 d <- config.detectors;
	 e <- config.extractors;
	 m <- config.matchers) yield {
      CorrespondenceExperiment(ic, oi, d, e, m)
    }
  }
}
