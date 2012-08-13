package nebula

import java.io.File
import javax.imageio.ImageIO

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}

case class CorrespondenceExperimentConfig(
  val imageClasses: List[String],
  val otherImages: List[Int],
  val detectors: List[DetectorMethod],
  val extractors: List[ExtractorMethod],
  val matchers: List[MatcherMethod])

object CorrespondenceExperimentConfig {
  def fromJSONFile(file: File): CorrespondenceExperimentConfig = {
    IO.fromJSONFileAbstract[CorrespondenceExperimentConfig](ExperimentIO.formats, file)
  }
}

case class CorrespondenceExperiment(
  val imageClass: String,
  val otherImage: Int,
  val detector: DetectorMethod,
  val extractor: ExtractorMethod,
  val matcher: MatcherMethod) extends Experiment {
  val parameterAbbreviations: List[String] = "IC OI D E M".split(" ").toList
  val parameterValues: List[String] = List(imageClass, 
					   otherImage.toString, 
					   detector.abbreviation, 
					   extractor.abbreviation, 
					   matcher.abbreviation)
  
  def stringMap = parameterAbbreviations.zip(parameterValues).toMap

  lazy val leftImageFile = Global.run[RuntimeConfig].childPath("data/%s/images/img1.bmp".format(imageClass))
  def leftImage = ImageIO.read(leftImageFile)
  lazy val rightImageFile = Global.run[RuntimeConfig].childPath("data/%s/images/img%s.bmp".format(imageClass, otherImage))
  def rightImage = ImageIO.read(rightImageFile)
  lazy val homographyFile = Global.run[RuntimeConfig].childPath("data/%s/homographies/H1to%sp".format(imageClass, otherImage))
  def homography = Homography.fromFile(homographyFile)
}

object CorrespondenceExperiment{
  def fromConfig(config: CorrespondenceExperimentConfig): List[CorrespondenceExperiment] = {
    for (ic <- config.imageClasses;
	 oi <- config.otherImages;
	 d <- config.detectors;
	 e <- config.extractors;
	 m <- config.matchers) yield {
      CorrespondenceExperiment(ic, oi, d, e, m)
    }
  }
}
