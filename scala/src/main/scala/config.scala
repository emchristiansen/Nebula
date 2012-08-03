import java.io.File
import xml._

case class ExperimentConfig(
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
  val backgroundIsCrossCondition: Boolean
)

object ExperimentConfig { 
  def fromXML(node: Node): ExperimentConfig = {
    def texts(key: String): List[String] = (node \ key).toList.map(_.text)

    def booleanText(key: String): Boolean = {
      val ts = texts(key)
      assert(ts.size == 1)
      ts.head.toBoolean
    }

    ExperimentConfig(
      texts("roi"), 
      texts("distance"),
      texts("pose"),
      booleanText("poseIsCrossCondition"),
      texts("illumination"),
      booleanText("illuminationIsCrossCondition"),
      texts("blur"),
      booleanText("blurIsCrossCondition"),
      texts("noise"),
      booleanText("noiseIsCrossCondition"),
      texts("jpeg"),
      booleanText("jpegIsCrossCondition"),
      texts("misalignment"),
      booleanText("misalignmentIsCrossCondition"),
      texts("background"),
      booleanText("backgroundIsCrossCondition")
    )
  }

  def fromFile(filename: String): ExperimentConfig = fromXML(XML.loadFile(filename))
}

case class RuntimeConfig(
  val piSliceRoot: String,
  val lfwRoot: String,
  val backgroundRoot: String,
  val sfsRoot: String,
  val scaleFactor: Double,
  val numFolds: Int,
  val numIdentities: Int,
  val deleteTemporaryFiles: Boolean,
  val parallel: Boolean,
  val maxSimultaneousExperiments: Int,
  val skipCompletedExperiments: Boolean,
  val tempDirectory: String
)
  
object RuntimeConfig {
  def fromXML(node: Node): RuntimeConfig = {
    def text = XMLUtil.text(node) _

    val homeString = Global.homeDirectory + "/"

    RuntimeConfig(
      homeString + text("piSliceRoot"), 
      homeString + text("lfwRoot"), 
      homeString + text("backgroundRoot"), 
      homeString + text("sfsRoot"), 
      text("scaleFactor").toDouble, 
      text("numFolds").toInt, 
      text("numIdentities").toInt, 
      text("deleteTemporaryFiles").toBoolean, 
      text("parallel").toBoolean, 
      text("maxSimultaneousExperiments").toInt, 
      text("skipCompletedExperiments").toBoolean,
      text("tempDirectory").toString
    )
  }

  def fromFile(filename: String): RuntimeConfig = fromXML(XML.loadFile(filename))
}

case class Condition(
  val pose: String,
  val illumination: String,
  val blur: String,
  val noise: String,
  val jpeg: String,
  val misalignment: String,
  val background: String) {
  def toXML: Node = {
    <condition>
      <pose>{pose}</pose>
      <illumination>{illumination}</illumination>
      <blur>{blur}</blur>
      <noise>{noise}</noise>
      <jpeg>{jpeg}</jpeg>
      <misalignment>{misalignment}</misalignment>
      <background>{background}</background>
    </condition>
  }
}

object Condition{
  def fromXML(node: Node): Condition = {
    def text = XMLUtil.text(node) _

    Condition(
      text("pose"),
      text("illumination"),
      text("blur"),
      text("noise"),
      text("jpeg"),
      text("misalignment"),
      text("background")
    )
  }

  def fromFile(filename: String): Condition = fromXML(XML.loadFile(filename))

  def validPair(left: Condition, right: Condition): Boolean = {
    (left.pose <= right.pose) && 
    (left.illumination <= right.illumination) &&
    (left.blur <= right.blur) && 
    (left.noise <= right.noise) && 
    (left.jpeg <= right.jpeg) && 
    (left.misalignment <= right.misalignment) && 
    (left.background <= right.background)
  }

  def respectsCrossCondition(config: ExperimentConfig, left: Condition, right: Condition): Boolean = { 
    val pose = config.poseIsCrossCondition || left.pose == right.pose
    val illumination = config.illuminationIsCrossCondition || left.illumination == right.illumination
    val blur = config.blurIsCrossCondition || left.blur == right.blur
    val noise = config.noiseIsCrossCondition || left.noise == right.noise
    val jpeg = config.jpegIsCrossCondition || left.jpeg == right.jpeg
    val misalignment = config.misalignmentIsCrossCondition || left.misalignment == right.misalignment
    val background = config.backgroundIsCrossCondition || left.background == right.background
    pose && illumination && blur && noise && jpeg && misalignment && background
  }
}

case class Experiment(
    val roi: String,
    val distance: String,
    val leftCondition: Condition,
    val rightCondition: Condition) {
  if (roi == "CFR") assert(leftCondition.pose == "051" && rightCondition.pose == "051")

  def toXML: Node = {
    <experiment>
      <roi>{roi}</roi>
      <distance>{distance}</distance>
      <leftCondition>{leftCondition.toXML}</leftCondition>
      <rightCondition>{rightCondition.toXML}</rightCondition>
    </experiment>
  }
 
  private val unixEpoch = System.currentTimeMillis / 1000L

  def filenameParts: List[String] = {
    val dateROIDistance: List[String] = List(unixEpoch.toString, roi, distance)

    def xPattern(left: String, right: String): String = left + "x" + right

    val pose = xPattern(leftCondition.pose, rightCondition.pose)
    val illumination = xPattern(leftCondition.illumination, rightCondition.illumination)
    val blur = xPattern(leftCondition.blur, rightCondition.blur)
    val noise = xPattern(leftCondition.noise, rightCondition.noise)
    val jpeg = xPattern(leftCondition.jpeg, rightCondition.jpeg)
    val misalignment = xPattern(leftCondition.misalignment, rightCondition.misalignment)
    val background = xPattern(leftCondition.background, rightCondition.background)

    val conditions: List[String] = List(pose, illumination, blur, noise, jpeg, misalignment, background)

    dateROIDistance ++ conditions
  }

  def filename: String = filenameParts.mkString("", "_", ".xml")

  def outDirectory = { 
    "%s/results/roc_data".format(Global.run.sfsRoot)
  }

  def outPath = { 
    "%s/%s".format(outDirectory, filename)
  }

  def filenameNoTime = filename.dropWhile(_ != '_').tail

  def pathNoTime = outDirectory + "/XXX_" + filenameNoTime

  def existingResultsFilename: String = {
    val results = new File(outDirectory).list.toList.map(_.toString)

    val filenames = results.filter(_.contains("_" + filenameNoTime)).toList

    if (filenames.size > 0) {
      // Take the filename last in the sort: the most recent one.
      filenames.sortBy(identity).reverse.head
    } else {
      ""
    }
  }

  def existingResultsPath: String = outDirectory + "/" + existingResultsFilename

  def alreadyRun = existingResultsFilename.size > 0
}

object Experiment{
  val parameterNames: List[String] = {
    "date roi distance pose illumination blur noise jpeg misalignment background".split(" ").toList
  }

  val parameterAbbreviations: List[String] = {
    "T R D P I L N J M B".split(" ").toList
  }

  def fromXML(node: Node): Experiment = {
    def text = XMLUtil.text(node) _

    def condition(name: String): Condition = {
      val list = (node \ name) \ "condition"
      assert(list.size == 1)
      Condition.fromXML(list.head)
    }

    Experiment(
      text("roi"),
      text("distance"),
      condition("leftCondition"),
      condition("rightCondition")
    )
  }

  def fromConfig(config: ExperimentConfig): List[Experiment] = { 
    val conditions = 
      for (p <- config.pose;
	   i <- config.illumination;
	   b <- config.blur;
	   n <- config.noise;
	   j <- config.jpeg;
	   m <- config.misalignment;
	   k <- config.background) yield { 
	Condition(p, i, b, n, j, m, k)
      }	 

    val validPairs = for (l <- conditions; 
			  r <- conditions; 
			  if Condition.validPair(l, r)) yield (l, r)

    val finalPairs = validPairs.filter({case (l, r) => Condition.respectsCrossCondition(config, l, r)})

    for (o <- config.roi;
	 d <- config.distance;
	 (l, r) <- finalPairs;
	 if ((o != "CFR" || (l.pose == "051" && r.pose == "051")) &&
	     (o != "SPB" || (l.pose == "240" && r.pose == "240")))) yield { 
      Experiment(o, d, l, r)
    }
  }
}

// case class TwoDimensionalTable(experimentGrid: List[List[Experiment]])

// object TwoDimensionalTable {
//   def fromXML(node: Node): TwoDimensionalTable = {
//     val experiments = Experiment.fromConfig(ExperimentConfig.fromXML((node \ "experimentConfig").head))

//     val yAxis = 

//     def optionalConstant(name: String): Option[String] = {
//       val matches = constants \ name
//       if (matches.size > 0) Some(matches.head.text)
//       else None
//     }

//     val roiConstant = optionalConstant("roi")
//     val distanceConstant = optionalConstant("distance")
//     val pose = optionalConstant
//     val illumination = optionalConstant("illumination")
//     val 

//     def text = XMLUtil.text(node) _

//     Condition(
//       text("pose"),
//       text("illumination"),
//       text("blur"),
//       text("noise"),
//       text("jpeg"),
//       text("misalignment"),
//       text("background")
//     )
//   }  

//   def fromFile(filename: String): TwoDimensionalTable = fromXML(XML.loadFile(filename))
// }
