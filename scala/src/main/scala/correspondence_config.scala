package nebula

import javax.imageio.ImageIO

case class CorrespondenceExperiment[D] (
  val imageClass: String,
  val otherImage: Int,
  val detector: Detector,
  val extractor: Extractor[D],
  val matcher: Matcher[D]) extends Experiment {
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
