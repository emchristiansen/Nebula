package nebula

import java.io.File

import org.apache.commons.io.FilenameUtils

// TODO: Duplication. Grr.
case class MPIERuntimeConfig(
  override val projectRoot: File,
  override val nebulaRoot: File,
  override val parallel: Boolean,
  override val tempDirectory: Option[File],
  override val deleteTemporaryFiles: Boolean,
  override val skipCompletedExperiments: Boolean,
  override val maxSimultaneousExperiments: Int,
  val runtimeConfig: RuntimeConfig,
  val piSliceRoot: String,
  val lfwRoot: String,
  val backgroundRoot: String,
  val scaleFactor: Double,
  val numFolds: Int,
  val numIdentities: Int) extends RuntimeConfigTrait

case class MPIEExperimentConfig(
  val roi: List[String],
  val distance: List[String],
  val pose: List[String],
  val poseIsCrossCondition: Boolean,
  val illumination: List[String],
  val illuminationIsCrossCondition: Boolean,
  val blur: List[String],
  val blurIsCrossCondition: Boolean,
  val noise: List[String],
  val noiseIsCrossCondition: Boolean,
  val jpeg: List[String],
  val jpegIsCrossCondition: Boolean,
  val misalignment: List[String],
  val misalignmentIsCrossCondition: Boolean,
  val background: List[String],
  val backgroundIsCrossCondition: Boolean)

case class MPIECondition(
  val pose: String,
  val illumination: String,
  val blur: String,
  val noise: String,
  val jpeg: String,
  val misalignment: String,
  val background: String)

object MPIECondition {
  def validPair(left: MPIECondition, right: MPIECondition): Boolean = {
    (left.pose <= right.pose) &&
      (left.illumination <= right.illumination) &&
      (left.blur <= right.blur) &&
      (left.noise <= right.noise) &&
      (left.jpeg <= right.jpeg) &&
      (left.misalignment <= right.misalignment) &&
      (left.background <= right.background)
  }

  def respectsCrossCondition(config: MPIEExperimentConfig,
                             left: MPIECondition,
                             right: MPIECondition): Boolean = {
    val pose = config.poseIsCrossCondition || left.pose == right.pose
    val illumination = config.illuminationIsCrossCondition ||
      left.illumination == right.illumination
    val blur = config.blurIsCrossCondition || left.blur == right.blur
    val noise = config.noiseIsCrossCondition || left.noise == right.noise
    val jpeg = config.jpegIsCrossCondition || left.jpeg == right.jpeg
    val misalignment = config.misalignmentIsCrossCondition ||
      left.misalignment == right.misalignment
    val background = config.backgroundIsCrossCondition ||
      left.background == right.background
    pose && illumination && blur && noise && jpeg && misalignment && background
  }
}

//case class MPIEExperiment(
//  val roi: String,
//  val distance: String,
//  val leftCondition: MPIECondition,
//  val rightCondition: MPIECondition) extends Experiment {
//  if (roi == "CFR") assert(leftCondition.pose == "051" && rightCondition.pose == "051")
//
//  val parameterAbbreviations: List[String] =
//    "R D P I L N J M B".split(" ").toList
//
//  val parameterValues = {
//    def xPattern(left: String, right: String): String = left + "x" + right
//
//    val pose = xPattern(leftCondition.pose, rightCondition.pose)
//    val illumination = xPattern(leftCondition.illumination,
//      rightCondition.illumination)
//    val blur = xPattern(leftCondition.blur, rightCondition.blur)
//    val noise = xPattern(leftCondition.noise, rightCondition.noise)
//    val jpeg = xPattern(leftCondition.jpeg, rightCondition.jpeg)
//    val misalignment = xPattern(leftCondition.misalignment,
//      rightCondition.misalignment)
//    val background = xPattern(leftCondition.background,
//      rightCondition.background)
//    val conditions =
//      List(pose, illumination, blur, noise, jpeg, misalignment, background)
//
//    List(roi, distance) ++ conditions
//  }
//}
//
//object MPIEExperiment {
//  val parameterNames: List[String] = {
//    "roi distance pose illumination blur noise jpeg misalignment background".split(" ").toList
//  }
//
//  def fromConfig(config: MPIEExperimentConfig): List[MPIEExperiment] = {
//    val conditions =
//      for (
//        p <- config.pose;
//        i <- config.illumination;
//        b <- config.blur;
//        n <- config.noise;
//        j <- config.jpeg;
//        m <- config.misalignment;
//        k <- config.background
//      ) yield {
//        MPIECondition(p, i, b, n, j, m, k)
//      }
//
//    val validPairs = for (
//      l <- conditions;
//      r <- conditions;
//      if MPIECondition.validPair(l, r)
//    ) yield (l, r)
//
//    val finalPairs = validPairs.filter({ case (l, r) => MPIECondition.respectsCrossCondition(config, l, r) })
//
//    for (
//      o <- config.roi;
//      d <- config.distance;
//      (l, r) <- finalPairs;
//      if ((o != "CFR" || (l.pose == "051" && r.pose == "051")) &&
//        (o != "SPB" || (l.pose == "240" && r.pose == "240")))
//    ) yield {
//      MPIEExperiment(o, d, l, r)
//    }
//  }
//}

case class MPIEProperties(val id: String, val session: String, val expression: String, val pose: String, val illumination: String) {
  assert(id.size == 3)
  assert(List("01", "02", "03", "04").contains(session))
  assert(expression == "01")
  assert(List("240", "190", "051").contains(pose))
  assert(List("00", "04", "06").contains(illumination))

  def pathSegment = {
    val poseUnderscore = pose match {
      case "240" => "24_0"
      case "190" => "19_0"
      case "051" => "05_1"
    }
    "session%s/multiview/%s/%s/%s".format(session, id, expression, poseUnderscore)
  }
}

object MPIEProperties {
  def parseMPIEPath(path: String): MPIEProperties = {
    val filename = FilenameUtils.removeExtension((new File(path)).getName)
    val Parser = """(\S+)_(\S+)_(\S+)_(\S+)_(\S+)""".r
    val Parser(id, session, expression, pose, illumination) = filename
    MPIEProperties(id, session, expression, pose, illumination)
  }
}
