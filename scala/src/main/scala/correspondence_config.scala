package nebula

import javax.imageio.ImageIO

//sealed trait CorrespondenceExperiment extends Experiment {
//  // TODO: Remove this.
//  def stringMap: Map[String,String]
//}

case class CorrespondenceExperiment(
  val imageClass: String,
  val otherImage: Int,
  val detector: Detector,
  val extractor: ExtractorParameterized[_],
  val matcher: MatcherParameterized[_],
  val descriptorConverter: _ => _) extends Experiment {
  // TODO: Remove this.
  val parameterAbbreviations: Seq[String] = "IC OI D E M".split(" ").toList
  val parameterValues: Seq[String] = List(
    imageClass,
    otherImage.toString,
    Util.abbreviate(detector),
    Util.abbreviate(extractor),
    Util.abbreviate(matcher))

  def stringMap = parameterAbbreviations.zip(parameterValues).toMap 
} 

object CorrespondenceExperiment {
  def apply(experiment: CorrespondenceExperimentParameterized[_, _]) =
    new CorrespondenceExperiment(
        experiment.imageClass,
        experiment.otherImage,
        experiment.detector,
        experiment.extractor,
        experiment.matcher,
        experiment.descriptorConverter)
}

//abstract class CorrespondenceExperiment(
//  val imageClass: String,
//  val otherImage: Int,
//  val detector: Detector,
//  val extractor: Extractor,
//  val matcher: Matcher) extends Experiment

case class CorrespondenceExperimentParameterized[MD, ED <% MD](
  val imageClass: String,
  val otherImage: Int,
  val detector: Detector,
  val extractor: ExtractorParameterized[ED],
  val matcher: MatcherParameterized[MD]) {
  val descriptorConverter = implicitly[ED => MD]
  
  
//  val extractorParameterized: ExtractorParameterized[_] = extractor match {
//    case x: ExtractorParameterized[_] => x
//  }
//  
//  val matcherParameterized: MatcherParameterized[_] = matcher match {
//    case x: MatcherParameterized[_] => x
//  }  
//  
//  val descriptorConverter: extractorParameterized.DescriptorType => matcherParameterized.DescriptorType

  val parameterAbbreviations: Seq[String] = "IC OI D E M".split(" ").toList
  val parameterValues: Seq[String] = List(
    imageClass,
    otherImage.toString,
    Util.abbreviate(detector),
    Util.abbreviate(extractor),
    Util.abbreviate(matcher))

  def stringMap = parameterAbbreviations.zip(parameterValues).toMap

  lazy val leftImageFile = Global.run[RuntimeConfig].projectChildPath("data/%s/images/img1.bmp".format(imageClass))
  def leftImage = ImageIO.read(leftImageFile)
  lazy val rightImageFile = Global.run[RuntimeConfig].projectChildPath("data/%s/images/img%s.bmp".format(imageClass, otherImage))
  def rightImage = ImageIO.read(rightImageFile)
  lazy val homographyFile = Global.run[RuntimeConfig].projectChildPath("data/%s/homographies/H1to%sp".format(imageClass, otherImage))
  def homography = Homography.fromFile(homographyFile)  
}

object CorrespondenceExperimentParameterized {
  def apply(experiment: CorrespondenceExperiment) = {
    type ExtractorType = experiment.extractor.DescriptorType
    type MatcherType = experiment.matcher.DescriptorType
    val extractor = experiment.extractor.asInstanceOf[ExtractorParameterized[ExtractorType]]
    val matcher = experiment.matcher.asInstanceOf[MatcherParameterized[MatcherType]]
    val converter = experiment.descriptorConverter.asInstanceOf[ExtractorType => MatcherType]
    
    new CorrespondenceExperimentParameterized(
        experiment.imageClass,
        experiment.otherImage,
        experiment.detector,
        extractor,
        matcher)(converter)
  }
}

//case class CorrespondenceExperiment(
//  val imageClass: String,
//  val otherImage: Int,
//  val detector: Detector,
//  val extractor: Extractor,
//  val matcher: Matcher) extends Experiment {
//  val parameterAbbreviations: Seq[String] = "IC OI D E M".split(" ").toList
//  val parameterValues: Seq[String] = List(
//    imageClass,
//    otherImage.toString,
//    Util.abbreviate(detector),
//    Util.abbreviate(extractor),
//    Util.abbreviate(matcher))
//
//  def stringMap = parameterAbbreviations.zip(parameterValues).toMap
//
//  lazy val leftImageFile = Global.run[RuntimeConfig].projectChildPath("data/%s/images/img1.bmp".format(imageClass))
//  def leftImage = ImageIO.read(leftImageFile)
//  lazy val rightImageFile = Global.run[RuntimeConfig].projectChildPath("data/%s/images/img%s.bmp".format(imageClass, otherImage))
//  def rightImage = ImageIO.read(rightImageFile)
//  lazy val homographyFile = Global.run[RuntimeConfig].projectChildPath("data/%s/homographies/H1to%sp".format(imageClass, otherImage))
//  def homography = Homography.fromFile(homographyFile)
//}
