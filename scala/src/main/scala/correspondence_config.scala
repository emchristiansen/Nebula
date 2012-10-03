package nebula

import javax.imageio.ImageIO

//trait CorrespondenceExperiment extends Experiment

class CorrespondenceExperiment(
  val imageClass: String,
  val otherImage: Int,
  val detector: Detector,
  val extractor: Extractor,
  val matcher: Matcher) extends Experiment {
  type DescriptorType
  
  // TODO: Uncomment
  //  // This block ensures the extractor and matcher types agree.
  //  def compareTypes[L <: Extractor, R <: Matcher](left: L, right: R)(
  //    implicit ev: L#DescriptorType =:= R#DescriptorType = null
  //  ): Boolean = ev != null
  //  assert(compareTypes(extractor, matcher))

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

case class CorrespondenceExperimentParameterized[D](
  override val imageClass: String,
  override val otherImage: Int,
  override val detector: Detector,
  override val extractor: ExtractorParameterized[D],
  override val matcher: MatcherParameterized[D]) extends CorrespondenceExperiment(
  imageClass,
  otherImage,
  detector,
  extractor,
  matcher) {
  override type DescriptorType = D
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
