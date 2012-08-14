package nebula

import java.io.File
import javax.imageio.ImageIO

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}

case class CorrespondenceExperiment[T <: AnyRef, D, E <: AnyRef, M <: AnyRef](
  val imageClass: String,
  val otherImage: Int,
  val detector: T,
  val extractor: E,
  val matcher: M)(
  implicit detectorLike: DetectorLike[T],
  extractorLike: ExtractorLike[E, D],
  matcherLike: MatcherLike[M, D]) extends Experiment {
  val parameterAbbreviations: Seq[String] = "IC OI D E M".split(" ").toList
  val parameterValues: Seq[String] = List(
    imageClass, 
    otherImage.toString, 
    Util.abbreviate(detector),
    Util.abbreviate(extractor),
    Util.abbreviate(matcher))
  
  def stringMap = parameterAbbreviations.zip(parameterValues).toMap

  lazy val leftImageFile = Global.run[RuntimeConfig].childPath("data/%s/images/img1.bmp".format(imageClass))
  def leftImage = ImageIO.read(leftImageFile)
  lazy val rightImageFile = Global.run[RuntimeConfig].childPath("data/%s/images/img%s.bmp".format(imageClass, otherImage))
  def rightImage = ImageIO.read(rightImageFile)
  lazy val homographyFile = Global.run[RuntimeConfig].childPath("data/%s/homographies/H1to%sp".format(imageClass, otherImage))
  def homography = Homography.fromFile(homographyFile)
}

object CorrespondenceExperiment {
  type CorrespondenceExperimentAbstract = CorrespondenceExperiment[_, _, _, _]
}
