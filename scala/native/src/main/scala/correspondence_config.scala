package nebula

import java.io.File
import javax.imageio.ImageIO

case class CorrespondenceExperimentConfig[D <: DescriptorTrait[_]](
  val imageClasses: List[String],
  val otherImages: List[Int],
  val detectors: List[DetectorMethod],
  val extractors: List[ExtractorMethod[D]],
  val matchers: List[MatcherMethod[D]])

// TODO: Remove this. See below.
case class CorrespondenceExperimentConfigOpaque(
  val imageClasses: List[String],
  val otherImages: List[Int],
  val detectors: List[DetectorMethod],
  val extractors: List[CorrespondenceMethod],
  val matchers: List[CorrespondenceMethod])

object CorrespondenceExperimentConfig {
  val instances = classOf[CorrespondenceExperimentConfigOpaque] :: DetectorMethod.instances ++ ExtractorMethod.instances ++ MatcherMethod.instances

  def fromJSONFile(file: File): List[CorrespondenceExperiment[_]] = {
    // This is a bit of a disaster. I like the CorrespondenceExperimentConfig
    // construct because it statically ensures the descriptor type agrees
    // between the extractors and matchers. However, I'm having trouble
    // deserializing from json things with type parameters. So we have this
    // ugly solution.
    val opaque = IO.fromJSONFileAbstract[CorrespondenceExperimentConfigOpaque](instances, file)
    val CorrespondenceExperimentConfigOpaque(imageClasses,
					     otherImages,
					     detectors,
					     extractors,
					     matchers) = opaque

    val (resolvedExtractors, resolvedMatchers) = extractors.zip(matchers).head match {
      case _: Tuple2[ExtractorMethod[SortDescriptor], MatcherMethod[SortDescriptor]] => {
	(extractors.asInstanceOf[List[ExtractorMethod[SortDescriptor]]],
	 matchers.asInstanceOf[List[MatcherMethod[SortDescriptor]]])
      }
      case _ => throw new Exception
    }

    val config = CorrespondenceExperimentConfig(imageClasses,
						otherImages,
						detectors,
						resolvedExtractors,
						resolvedMatchers)
    CorrespondenceExperiment.fromConfig(config)
//    throw new Exception
  }
}

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

  lazy val leftImageFile = Global.run[RuntimeConfig].childPath("data/%s/images/img1.bmp".format(imageClass))
  def leftImage = ImageIO.read(leftImageFile)
  lazy val rightImageFile = Global.run[RuntimeConfig].childPath("data/%s/images/img%s.bmp".format(imageClass, otherImage))
  def rightImage = ImageIO.read(rightImageFile)
  lazy val homographyFile = Global.run[RuntimeConfig].childPath("data/%s/homographies/H1to%sp".format(imageClass, otherImage))
  def homography = Homography.fromFile(homographyFile)
}

object CorrespondenceExperiment{
  // def fromConfig[D <: DescriptorTrait[_]](config: CorrespondenceExperimentConfig[D]): List[CorrespondenceExperiment[D]] = {
  def fromConfig(config: CorrespondenceExperimentConfig[DescriptorTrait[_]]): List[CorrespondenceExperiment[_]] = {
    for (ic <- config.imageClasses;
	 oi <- config.otherImages;
	 d <- config.detectors;
	 e <- config.extractors;
	 m <- config.matchers) yield {
      CorrespondenceExperiment(ic, oi, d, e, m)
    }
  }
}
