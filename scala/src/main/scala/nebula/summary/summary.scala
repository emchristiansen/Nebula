package nebula.summary

import nebula.graveyard._
import nebula.mpie._
import nebula.summary._
import nebula.smallBaseline._
import nebula.util._
import nebula.util.imageProcessing._
import nebula.wideBaseline._
import nebula._

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import org.opencv.features2d.DMatch

//import wideBaseline.WideBaselineExperimentSummary._
//import smallBaseline.SmallBaselineExperimentSummary._

///////////////////////////////////////////////////////////////////////////////

case class ExperimentSummary(
    summaryNumbers: Map[String, () => Double],
    summaryImages: Map[String, () => BufferedImage])

object ExperimentSummary {
  implicit class OutDirectory(self: ExperimentSummary) {
  def outDirectory(implicit runtime: RuntimeConfig) = runtime.projectChildPath(
    "summary")    
  }
}    

object SummaryUtil {
  def recognitionRate(dmatches: Seq[DMatch]): Double = {
    // The base image feature index is |queryIdx|, and the other 
    // image is |trainIdx|. This weirdness is caused by a convention
    // clash.
    val groupedByLeft = dmatches.groupBy(_.queryIdx)
    val groups = groupedByLeft.values.map(_.sortBy(_.distance))
    val numCorrect = groups.count(group => group.head.queryIdx == group.head.trainIdx)
    numCorrect.toDouble / groups.size
  }

  // TODO: More general than WideBaseline
  def experimentTable[D, E, M, F](
    baseExperiment: WideBaselineExperiment[D, E, M, F],
    rowMutations: Seq[WideBaselineExperiment[D, E, M, F] => WideBaselineExperiment[D, E, M, F]],
    columnMutations: Seq[WideBaselineExperiment[D, E, M, F] => WideBaselineExperiment[D, E, M, F]]): Seq[Seq[WideBaselineExperiment[D, E, M, F]]] = {
    for (row <- rowMutations) yield {
      for (column <- columnMutations) yield {
        column(row(baseExperiment))
      }
    }
  }

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
}

