package nebula

import java.io.File

import com.googlecode.javacv.cpp.opencv_features2d._


object Summary {
  def recognitionRate(dmatches: List[DMatch]): Double = {
    // The base image feature index is |queryIdx|, and the other 
    // image is |trainIdx|. This weirdness is caused by a convention
    // clash.
    val groupedByLeft = dmatches.groupBy(_.queryIdx)
    val groups = groupedByLeft.values.map(_.sortBy(_.distance))
    val numCorrect = groups.count(group => group.head.queryIdx == group.head.trainIdx)
    numCorrect.toDouble / groups.size
  }

  def resultsTable(
    baseExperiment: CorrespondenceExperiment,
    rowMutations: Seq[CorrespondenceExperiment => CorrespondenceExperiment],
    columnMutations: Seq[CorrespondenceExperiment => CorrespondenceExperiment]): Seq[Seq[CorrespondenceExperimentResults]] = {
    (for (row <- Util.parallelize(rowMutations)) yield {
      (for (column <- Util.parallelize(columnMutations)) yield {
        val experiment = column(row(baseExperiment))
        CorrespondenceExperimentResults.fromExperiment(experiment)
      }).toList
    }).toList
  }
}

object SummaryUtil {
  // Turns Set(
  // Map(1 -> 12, 2 -> 13),
  // Map(1 -> 10, 3 -> 10, 2 -> 13))
  // into
  // Map(1 -> Set(12, 10), 2 -> Set(13), 3 -> Set(10))
  def mapUnion[A, B](maps: Set[Map[A, B]]): Map[A, Set[B]] = {
    val set = maps.map(_.toSeq).flatten.toSet
    val groups = set.groupBy(_._1)
    groups.mapValues(_.map(_._2))
  }

  // Turns Seq(
  // Map(1 -> 10, 2 -> 20),
  // Map(1 -> 10, 2 -> 30))
  // into
  // Seq(Map(2 -> 20), Map(2 -> 30)).
  def changingFields[A, B](maps: Seq[Map[A, B]]): Seq[Map[A, B]] = {
    val fields = mapUnion(maps.toSet).filter(_._2.size > 1).keys.toSet
    maps.map(_.filterKeys(fields))
  }

  def summarizeStructure(maps: Set[Map[String, String]]): String = {
    val union = mapUnion(maps)
    val constantPairs = union.filter(_._2.size == 1).mapValues(_.head).toSet
    val variablePairs = {
      val keys = union.filter(_._2.size > 1).keys.toSet
      keys.map(k => k -> "*").toMap
    }
    val summaryMap = constantPairs ++ variablePairs
    val components = summaryMap.toSeq.sortBy(_._1).map({ case (k, v) => "%s-%s".format(k, v) })
    components.mkString("_")
  }

  def tableTitle(experiments: Seq[CorrespondenceExperiment]): String = {
    val maps = experiments.map(_.stringMap).toSet
    summarizeStructure(maps)
  }

  def tableEntryTitles(experiments: Seq[CorrespondenceExperiment]): Seq[String] = {
    val experimentMaps = experiments.map(_.stringMap)
    val union = mapUnion(experimentMaps.toSet)
    val variableKeys = union.filter(_._2.size > 1).keys.toSet

    def entryTitle(experimentMap: Map[String, String]): String = {
      experimentMap.filterKeys(variableKeys).toSeq.map({ case(k, v) => "%s-%s".format(k, v) }).mkString("_")
    }

    experimentMaps.map(entryTitle)
  }
}

// case class TwoDimensionalTable(experimentGrid: List[List[MPIEExperiment]])

// object TwoDimensionalTable {
//   def fromXML(node: Node): TwoDimensionalTable = {
//     val experiments = MPIEExperiment.fromConfig(MPIEExperimentConfig.fromXML((node \ "experimentConfig").head))

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

// case class ROC(val resultsData: ResultsData) {
//   val (predictions, truths) = resultsData.predictionsAndTruths.map({case PredictionAndTruth(p, t) => (p, t)}).unzip

//   assert(predictions.sorted == predictions)
//   assert(predictions.size == truths.size)

//   lazy val curve: List[Tuple2[Double, Double]] = {
//     val truthInts = truths.map(b => if (b) 1 else 0)

//     def heads[A](list: List[A]): List[List[A]] = list.inits.toList.reverse

//     val pairs = for (guessPositive <- heads(truthInts)) yield {
//       val truePositives = guessPositive.sum
//       val falsePositives = guessPositive.size - truePositives

//       val numPositive = truthInts.sum
//       val numNegative = truthInts.size - numPositive      

//       val truePositiveRate = truePositives.toDouble / numPositive.toDouble
//       val falsePositiveRate = falsePositives.toDouble / numNegative.toDouble
//       (falsePositiveRate, truePositiveRate)
//     }
//     assert(pairs.sortBy(_._1) == pairs)

//     // For each fpr, we will record only the highest tpr.
//     val reducedPairs = {
//       // The tprs are in ascending order, so the map will retain only the highest tprs.
//       val map = pairs.toMap
//       for (k <- map.keys.toList.sorted) yield (k, map(k))
//     }

//     reducedPairs
//   }

//   def tprFromFPR(fpr: Double): Double = {
//     val map = curve.toMap
//     if (map.contains(fpr)) { 
//       map(fpr)
//     } else { 
//       val (smaller, bigger) = curve.partition(_._1 < fpr)
//       val (smallerFPR, smallerTPR) = smaller.last
//       val (biggerFPR, biggerTPR) = bigger.head

//       val smallerDistance = fpr - smallerFPR
//       val biggerDistance = biggerFPR - fpr

//       val smallerWeight = biggerDistance / (biggerFPR - smallerFPR)
//       val biggerWeight = 1 - smallerWeight

//       smallerWeight * smallerTPR + biggerWeight * biggerTPR
//     }
//   }
// }

// case class ExperimentNamer(experiments: List[MPIEExperiment]) {
//   // Drop the first value, the time the experiment was performed.
//   val parameterSets = experiments.map(_.filenameParts).transpose.map(_.toSet).tail

//   def setToString(set: Set[String]) = set.toList.mkString("-")

//   val title = parameterSets.map(setToString).mkString(" ")

//   val variableParameters = parameterSets.map(_.size > 1)

//   def curveName(experiment: MPIEExperiment): String = {
//     val nameValuePairs = MPIEExperiment.parameterAbbreviations.zip(experiment.filenameParts).tail
//     val namedParameters = nameValuePairs.map({case (n, v) => n + ":" + v})

//     // We only care about parameters which vary.
//     val name = variableParameters.zip(namedParameters).filter(_._1).map(_._2).mkString(" ")
//     if (name.size > 0) name else "no comparison"
//   }
// }

// object MeanROC {
//   def meanROCCurve(resultsDataList: List[ResultsData]): List[Tuple2[Double, Double]] = { 
//     val rocList = resultsDataList.map(ROC)

//     val allFPRs = rocList.map(_.curve).flatten.map(_._1).distinct.sorted

//     def meanTPR(fpr: Double) = { 
//       rocList.map(_.tprFromFPR(fpr)).sum / rocList.size
//     }

//     val allTPRs = allFPRs.map(f => meanTPR(f))
//     allFPRs.zip(allTPRs)
//   }

//   def drawCompletedExperiments(experiments: List[MPIEExperiment]) {
//     val namer = ExperimentNamer(experiments)

//     val results = experiments.map(MPIEExperimentResults.fromCompletedExperiment)

//     def curveInfo(result: MPIEExperimentResults) = {
//       val name = namer.curveName(result.experiment)
//       val (fprs, tprs) = meanROCCurve(result.resultsDataList).unzip
//       val fprString = fprs.mkString(" ")
//       val tprString = tprs.mkString(" ")
//       List(name, fprString, tprString).mkString("\n")
//     }

//     val tempContents = (List(namer.title) ++ results.map(curveInfo)).mkString("\n")
//     val tempFile = File.createTempFile("rocdata", ".txt")
//     if (Global.run.deleteTemporaryFiles) tempFile.deleteOnExit
//     org.apache.commons.io.FileUtils.writeStringToFile(tempFile, tempContents)

//     val filename = namer.title.replace(" ", "_") + ".png"
//     val outPath = "%s/results/roc_curves/%s".format(Global.run.sfsRoot, filename)

//     val pythonScript = Global.run.sfsRoot + "/src/python/single_roc.py"
//     val command = "python %s %s %s".format(pythonScript, tempFile, outPath)
//     assert(sys.process.Process(command).! == 0)

//     println("wrote %s".format(outPath))
//   }
// }

// object EER {
//   def calculateSingleCurve(curve: List[Tuple2[Double, Double]]): Double = {
//     val eers: List[Double] = for (List((leftFPR, leftTPR), (rightFPR, rightTPR)) <- curve.sliding(2).toList;
// 				  leftFNR = 1 - leftTPR;
// 				  rightFNR = 1 - rightTPR;
// 				  if (leftFPR <= leftFNR && rightFPR >= rightFNR)) yield {
// 				    assert(leftFPR <= rightFPR)
// 				    assert(leftTPR <= rightTPR)
// 				    (leftFPR + leftFNR + rightFPR + rightFNR) / 4.0
//     }
//     val eer = eers.head
//     assert(eer >= 0 && eer <= 1)
//     eer
//   }

//   def calculate(experiment: MPIEExperiment): Tuple2[Double, Double] = {
//     val results = MPIEExperimentResults.fromCompletedExperiment(experiment)
//     val eers = results.resultsDataList.map(ROC).map(r => calculateSingleCurve(r.curve))
//     val stats = new org.apache.commons.math.stat.descriptive.DescriptiveStatistics(eers.toArray)
//     (stats.getMean, stats.getStandardDeviation / math.sqrt(eers.size))
//   }

//   def eerTable(experiments: List[MPIEExperiment]): String = {
//     val header = (MPIEExperiment.parameterNames ++ List("eer")).mkString("\t")
//     val rows = for (experiment <- experiments.par) yield {
//       val (mean, std) = EER.calculate(experiment)
//       (experiment.filenameParts ++ List(mean, std)).mkString("\t")
//     }
//     (header :: rows.toList).mkString("\n")
//   }

//   def writeEERTableAtPath(experiments: List[MPIEExperiment], path: String) {
//     val table = eerTable(experiments)
//     org.apache.commons.io.FileUtils.writeStringToFile(new File(path), table)
//   }

//   def writeEERTable(experiments: List[MPIEExperiment]) {
//     val namer = ExperimentNamer(experiments)
//     val filename = namer.title.replace(" ", "_") + ".csv"
//     val tablePath = "%s/results/eer_tables/%s".format(Global.run.sfsRoot, filename)
//     writeEERTableAtPath(experiments, tablePath)
//   }

//   lazy val fullTablePath = "%s/results/eer_table_full.csv".format(Global.run.sfsRoot)
// }

// case class TableEntry(val roi: String, val distance: String, val pose: String, val illumination: String, val blur: String, val noise: String, val jpeg: String, val misalignment: String, val background: String, val eer: Double, val std: Double)

// object TableEntry {
//   def fromLine(line: String): TableEntry = {
//     val split = line.split("\t").toList.tail
//     assert(split.size == 11)
//     TableEntry(split(0), split(1), split(2), split(3), split(4), split(5), split(6), split(7), split(8), split(9).toDouble, split(10).toDouble)
//   }
// }

// object EERTable {
//   def formatTableLatex(table: List[List[String]]): String = {
//     table.map(_.mkString(" & ")).mkString("""\hline""" + "\n", """\\ \hline""" + "\n", """\\ \hline""")
//   }

//   def formatMulticolumnTableLatex(table: List[List[String]]): String = {
//     assert(table.size == 6)
//     assert(table(0).size == 10)
//     val strings = table.map(_.mkString(" & "))
// """
// \begin{tabular}{cc|c|c|c|c|c|c|c|c|c|c|l}
// \cline{3-12}
// & & \multicolumn{10}{|c|}{\texttt{roi}} \\ \cline{3-12}
// & & CFR & TFR & FE & FR & HIE & HIR & HEE & HER & HNR & PR \\ \cline{1-12}
// \multicolumn{1}{|c|}{\multirow{3}{*}{synthetic}} &
// \multicolumn{1}{|c|}{TODO} & %s
// \\ \cline{2-12}
// \multicolumn{1}{|c|}{}                        &
// \multicolumn{1}{|c|}{TODO} & %s
// \\ \cline{2-12}
// \multicolumn{1}{|c|}{}                        &
// \multicolumn{1}{|c|}{TODO} & %s
// \\ \cline{1-12}
// \multicolumn{1}{|c|}{\multirow{3}{*}{blank}} &
// \multicolumn{1}{|c|}{TODO} & %s
// \\ \cline{2-12}
// \multicolumn{1}{|c|}{}                        &
// \multicolumn{1}{|c|}{TODO} & %s
// \\ \cline{2-12}
// \multicolumn{1}{|c|}{}                        &
// \multicolumn{1}{|c|}{TODO} & %s
// \\ \cline{1-12}
// \end{tabular}
// """.format(strings(0), strings(1), strings(2), strings(3), strings(4), strings(5))
//   }

//   def distanceToCode(distance: String): String = distance match {
//     case "LBP" => "L"
//     case "LBPWolf" => "W"
//     case "SSIM" => "S"
//     case "Eigenface" => "E"
//     case "Sparseface" => "P"
//     case "L1" => "1"
//     case "L2" => "2"
//   }

//   val rois = List("CFR", "TFR", "FE", "FR", "HIE", "HIR", "HEE", "HER", "HNR", "PR")
//   val poses = List("051x051", "190x190", "240x240")
//   //val illuminations = List("00x00", "0.25xx0.25x", "4.0xx4.0x", "04x04", "06x06")
//   val illuminations = List("00x00", "0.25xx0.25x", "4.0xx4.0x")
//   val blurs = List("0x0", "2x2", "8x8")
//   val noises = List("0x0", "20x20", "80x80")
//   val misalignments = List("0x0", "3x3", "12x12")
//   val backgrounds = List("syntheticxsynthetic", "blankxblank")
//   val jpegs = List("0x0", "0.1x0.1", "0.001x0.001")
//   val distances = List("LBP", "Eigenface", "L1", "L2")

//   def mkTable(cullFilter: (TableEntry => Boolean), conditionFilter: ((String, TableEntry) => Boolean), conditions: List[String], outFilename: String) {
//     val lines = io.Source.fromFile(EER.fullTablePath).mkString.split("\n").filter(_.size > 0).tail
//     val entries = lines.map(TableEntry.fromLine)

//     // TODO
//     val culled = entries.filter(cullFilter)//.filter(_.distance != "LBPWolf")

//     culled.foreach(println)

//     val table = for (b <- backgrounds; p <- conditions) yield {
//       for (r <- rois) yield {
// 	val remaining = culled.filter(e => e.background == b && e.roi == r).filter(e => conditionFilter(p, e))
// 	if (remaining.size == 0) { 
// 	  "N/A" 
// 	} else {
// //	  val best = remaining.sortBy(_.eer).reverse.head
// 	  val best = remaining.sortBy(_.eer).head
// 	  """\begin{tabular}{c}$%.2f_\text{%s}$\vspace{-2pt}\\\scriptsize{$\pm %.2f$}\end{tabular}""".format(best.eer, distanceToCode(best.distance), best.std)
// 	}
//       }
//     }

//     val outPath = "%s/results/2d_eer_tables/%s.csv".format(Global.run.sfsRoot, outFilename)
//     val tableString = formatMulticolumnTableLatex(table)
//     val outString = List(backgrounds.mkString(" "), conditions.mkString(" "), rois.mkString(" "), tableString).mkString("\n")
//     org.apache.commons.io.FileUtils.writeStringToFile(new File(outPath), outString)    
//   }

//   def poseBestDistance {
//     def cullFilter(e: TableEntry): Boolean = e.illumination == "00x00" && e.blur == "0x0" && e.noise == "0x0" && e.jpeg == "0x0" && e.misalignment == "0x0"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.pose == value
//     val conditions = poses
//     val outFilename = "poseBestDistance"
//     mkTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def poseNaturalBestDistance {
//     def cullFilter(e: TableEntry): Boolean = e.illumination == "00x00" && e.blur == "2x2" && e.noise == "20x20" && e.jpeg == "0x0" && e.misalignment == "3x3"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.pose == value
//     val conditions = poses
//     val outFilename = "poseNaturalBestDistance"
//     mkTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def blurBestDistance {
//     def cullFilter(e: TableEntry): Boolean = e.illumination == "00x00" && e.pose == "051x051" && e.noise == "0x0" && e.jpeg == "0x0" && e.misalignment == "0x0"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.blur == value
//     val conditions = blurs
//     val outFilename = "blurBestDistance"
//     mkTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def blurProfileBestDistance {
//     def cullFilter(e: TableEntry): Boolean = e.illumination == "00x00" && e.pose == "240x240" && e.noise == "0x0" && e.jpeg == "0x0" && e.misalignment == "0x0"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.blur == value
//     val conditions = blurs
//     val outFilename = "blurProfileBestDistance"
//     mkTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def noiseBestDistance {
//     def cullFilter(e: TableEntry): Boolean = e.illumination == "00x00" && e.pose == "051x051" && e.blur == "0x0" && e.jpeg == "0x0" && e.misalignment == "0x0"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.noise == value
//     val conditions = noises
//     val outFilename = "noiseBestDistance"
//     mkTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def noiseProfileBestDistance {
//     def cullFilter(e: TableEntry): Boolean = e.illumination == "00x00" && e.pose == "240x240" && e.blur == "0x0" && e.jpeg == "0x0" && e.misalignment == "0x0"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.noise == value
//     val conditions = noises
//     val outFilename = "noiseProfileBestDistance"
//     mkTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def misalignmentBestDistance {
//     def cullFilter(e: TableEntry): Boolean = e.illumination == "00x00" && e.pose == "051x051" && e.blur == "0x0" && e.jpeg == "0x0" && e.noise == "0x0"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.misalignment == value
//     val conditions = misalignments
//     val outFilename = "misalignmentBestDistance"
//     mkTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def misalignmentProfileBestDistance {
//     def cullFilter(e: TableEntry): Boolean = e.illumination == "00x00" && e.pose == "240x240" && e.blur == "0x0" && e.jpeg == "0x0" && e.noise == "0x0"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.misalignment == value
//     val conditions = misalignments
//     val outFilename = "misalignmentProfileBestDistance"
//     mkTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def illuminationBestDistance {
//     def cullFilter(e: TableEntry): Boolean = e.pose == "051x051" && e.blur == "0x0" && e.jpeg == "0x0" && e.noise == "0x0" && e.misalignment == "0x0"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.illumination == value
//     val conditions = illuminations
//     val outFilename = "illuminationBestDistance"
//     mkTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def illuminationProfileBestDistance {
//     def cullFilter(e: TableEntry): Boolean = e.pose == "240x240" && e.blur == "0x0" && e.jpeg == "0x0" && e.noise == "0x0" && e.misalignment == "0x0"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.illumination == value
//     val conditions = illuminations
//     val outFilename = "illuminationProfileBestDistance"
//     mkTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def jpegBestDistance {
//     def cullFilter(e: TableEntry): Boolean = e.illumination == "00x00" && e.pose == "051x051" && e.blur == "0x0" && e.noise == "0x0" && e.misalignment == "0x0"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.jpeg == value
//     val conditions = jpegs
//     val outFilename = "jpegBestDistance"
//     mkTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def lbpNeutralRoiScores {
//     def cullFilter(e: TableEntry): Boolean = e.illumination == "00x00" && e.distance == "LBP" && e.blur == "0x0" && e.jpeg == "0x0" && e.noise == "0x0" && e.misalignment == "0x0"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.pose == value
//     val conditions = poses
//     val outFilename = "lbpRoiScores"
//     mkTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def eigenfaceNeutralRoiScores {
//     def cullFilter(e: TableEntry): Boolean = e.illumination == "00x00" && e.distance == "Eigenface" && e.blur == "0x0" && e.jpeg == "0x0" && e.noise == "0x0" && e.misalignment == "0x0"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.pose == value
//     val conditions = poses
//     val outFilename = "eigenfaceRoiScores"
//     mkTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def l1NeutralRoiScores {
//     def cullFilter(e: TableEntry): Boolean = e.illumination == "00x00" && e.distance == "L1" && e.blur == "0x0" && e.jpeg == "0x0" && e.noise == "0x0" && e.misalignment == "0x0"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.pose == value
//     val conditions = poses
//     val outFilename = "l1RoiScores"
//     mkTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def l2NeutralRoiScores {
//     def cullFilter(e: TableEntry): Boolean = e.illumination == "00x00" && e.distance == "L2" && e.blur == "0x0" && e.jpeg == "0x0" && e.noise == "0x0" && e.misalignment == "0x0"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.pose == value
//     val conditions = poses
//     val outFilename = "l2RoiScores"
//     mkTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def ssimNeutralRoiScores {
//     def cullFilter(e: TableEntry): Boolean = e.illumination == "00x00" && e.distance == "SSIM" && e.blur == "0x0" && e.jpeg == "0x0" && e.noise == "0x0" && e.misalignment == "0x0"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.pose == value
//     val conditions = poses
//     val outFilename = "ssimRoiScores"
//     mkTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def realisticRoiScores {
//     def cullFilter(e: TableEntry): Boolean = e.illumination == "00x00" && e.pose == "051x051" && e.blur == "2x2" && e.noise == "20x20" && e.jpeg == "0x0"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.misalignment == value
//     val conditions = misalignments
//     val outFilename = "realisticRoiScores"
//     mkTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def realisticProfileRoiScores {
//     def cullFilter(e: TableEntry): Boolean = e.illumination == "00x00" && e.pose == "240x240" && e.blur == "2x2" && e.noise == "20x20" && e.jpeg == "0x0"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.misalignment == value
//     val conditions = misalignments
//     val outFilename = "realisticProfileRoiScores"
//     mkTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def mkPerDistanceTable(cullFilter: (TableEntry => Boolean), conditionFilter: ((String, TableEntry) => Boolean), conditions: List[String], outFilename: String) {
//     val lines = io.Source.fromFile(EER.fullTablePath).mkString.split("\n").filter(_.size > 0).tail
//     val entries = lines.map(TableEntry.fromLine)

//     // TODO
//     val culled = entries.filter(cullFilter)//.filter(_.distance != "LBPWolf")

//     culled.foreach(println)

//     val table = for (b <- backgrounds; p <- conditions) yield {
//       for (r <- rois) yield {
// 	val remaining = culled.filter(e => e.background == b && e.roi == r).filter(e => conditionFilter(p, e))
// 	if (remaining.size == 0) { 
// 	  "N/A" 
// 	} else {
// 	  val best = remaining.sortBy(_.eer).head
// 	  // val (bestEER, bestDistance) = (best.eer, best.distance)
// 	  //   "$%.2f_\\text{%s}$".format(bestEER, distanceToCode(bestDistance))
// 	  """\begin{tabular}{c}$%.2f_\text{%s}$\vspace{-2pt}\\\scriptsize{$\pm %.2f$}\end{tabular}""".format(best.eer, distanceToCode(best.distance), best.std)
// 	}
//       }
//     }

//     val outPath = "%s/results/2d_eer_tables/%s.csv".format(Global.run.sfsRoot, outFilename)

//     val tableString = formatEightRowMulticolumnTableLatex(table)
//     val outString = List(backgrounds.mkString(" "), conditions.mkString(" "), rois.mkString(" "), tableString).mkString("\n")
//     org.apache.commons.io.FileUtils.writeStringToFile(new File(outPath), outString)    
//   }

//   def formatEightRowMulticolumnTableLatex(table: List[List[String]]): String = {
//     assert(table.size == 8)
//     assert(table(0).size == 10)
//     val strings = table.map(_.mkString(" & "))
// """
// \begin{tabular}{cc|c|c|c|c|c|c|c|c|c|c|l}
// \cline{3-12}
// & & \multicolumn{10}{|c|}{\texttt{roi}} \\ \cline{3-12}
// & & CFR & TFR & FE & FR & HIE & HIR & HEE & HER & HNR & PR \\ \cline{1-12}
// \multicolumn{1}{|c|}{\multirow{4}{*}{synthetic}} &
// \multicolumn{1}{|c|}{TODO} & %s
// \\ \cline{2-12}
// \multicolumn{1}{|c|}{}                        &
// \multicolumn{1}{|c|}{TODO} & %s
// \\ \cline{2-12}
// \multicolumn{1}{|c|}{}                        &
// \multicolumn{1}{|c|}{TODO} & %s
// \\ \cline{2-12}
// \multicolumn{1}{|c|}{}                        &
// \multicolumn{1}{|c|}{TODO} & %s
// \\ \cline{1-12}
// \multicolumn{1}{|c|}{\multirow{4}{*}{blank}} &
// \multicolumn{1}{|c|}{TODO} & %s
// \\ \cline{2-12}
// \multicolumn{1}{|c|}{}                        &
// \multicolumn{1}{|c|}{TODO} & %s
// \\ \cline{2-12}
// \multicolumn{1}{|c|}{}                        &
// \multicolumn{1}{|c|}{TODO} & %s
// \\ \cline{2-12}
// \multicolumn{1}{|c|}{}                        &
// \multicolumn{1}{|c|}{TODO} & %s
// \\ \cline{1-12}
// \end{tabular}
// """.format(strings(0), strings(1), strings(2), strings(3), strings(4), strings(5),
// 	 strings(6), strings(7))
//   }

//   def distanceNeutralRoiScores {
//     def cullFilter(e: TableEntry): Boolean = e.pose == "051x051" && e.illumination == "00x00" && e.blur == "0x0" && e.jpeg == "0x0" && e.noise == "0x0" && e.misalignment == "0x0"
//     //def cullFilter(e: TableEntry): Boolean = e.illumination == "00x00" && e.blur == "0x0" && e.jpeg == "0x0" && e.noise == "0x0" && e.misalignment == "0x0" && e.background == "syntheticxsynthetic"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.distance == value
//     val conditions = distances
//     //val conditions = poses
//     val outFilename = "distanceNeutralRoiScores"
//     mkPerDistanceTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def distanceProfileRoiScores {
//     def cullFilter(e: TableEntry): Boolean = e.pose == "240x240" && e.illumination == "00x00" && e.blur == "0x0" && e.jpeg == "0x0" && e.noise == "0x0" && e.misalignment == "0x0"
//     //def cullFilter(e: TableEntry): Boolean = e.illumination == "00x00" && e.blur == "0x0" && e.jpeg == "0x0" && e.noise == "0x0" && e.misalignment == "0x0" && e.background == "syntheticxsynthetic"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.distance == value
//     val conditions = distances
//     //val conditions = poses
//     val outFilename = "distanceProfileRoiScores"
//     mkPerDistanceTable(cullFilter, conditionFilter, conditions, outFilename)
//   }

//   def lbpMisalignRoiScores {
//     def cullFilter(e: TableEntry): Boolean = e.illumination == "00x00" && e.pose == "240x240" && e.blur == "0x0" && e.jpeg == "0x0" && e.noise == "0x0" && e.distance == "LBP"
//     def conditionFilter(value: String, e: TableEntry): Boolean = e.misalignment == value
//     val conditions = misalignments
//     val outFilename = "misalignmentLbpDistance"
//     mkTable(cullFilter, conditionFilter, conditions, outFilename)
//   }
// }
