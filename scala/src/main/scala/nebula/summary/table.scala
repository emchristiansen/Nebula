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
import breeze.linalg.DenseMatrix

import MathUtil._
import DenseMatrixUtil._

import SummaryUtil._
import grizzled.math._
import DetectorJsonProtocol._
import ExtractorJsonProtocol._
import MatcherJsonProtocol._
import ExperimentJsonProtocol._
import ExperimentResultsJsonProtocol._
import spray.json._

///////////////////////////////////////////////////////////////////////////////

case class Table[A](
  title: String,
  rowLabels: IndexedSeq[String],
  columnLabels: IndexedSeq[String],
  entries: DenseMatrix[A]) {
  lazy val unixEpoch = System.currentTimeMillis / 1000L
}

object Table {
  implicit class AddNormalize(self: Table[Double]) {
    def normalizeColumns: Table[Double] = {
      val transpose = self.entries.toSeqSeq.transpose
      val transposeNormalized = transpose.map(
          row => row.map(_ / stats.mean(row: _*)))
      self.copy(entries = transposeNormalized.transpose.toMatrix)
    }
  }
  
  implicit class MoreFunctions[A](self: Table[A]) {
    def toTSV(toString: A => String): String = {
      val topRow = self.title +: self.columnLabels
      val stringEntries: Seq[Seq[String]] = self.entries.map(toString).toSeqSeq
      val otherRows: Seq[Seq[String]] = self.rowLabels.zip(stringEntries).map({
        case (title, entries) => Seq(title) ++ entries
      })
      val stringsTable = Seq(topRow) ++ otherRows

      stringsTable.map(_.mkString("\t")).mkString("\n")
    }

    def path(implicit runtime: RuntimeConfig): File = runtime.projectChildPathNew(
      "summary/%s_%s.csv".format(self.unixEpoch, self.title))
  }

  def title(experiments: Seq[Experiment]): String = {
    val maps = experiments.map(_.toJson).map(JSONUtil.getParametersFromJson).toSet
    summarizeStructure(maps)
  }

  def entryTitles(experiments: Seq[Experiment]): IndexedSeq[String] = {
    val experimentMaps = experiments.map(_.toJson).map(JSONUtil.getParametersFromJson)
    val union = mapUnion(experimentMaps.toSet)
    val variableKeys = union.filter(_._2.size > 1).keys.toSet

    def entryTitle(experimentMap: Map[String, String]): String = {
      experimentMap.filterKeys(variableKeys).toSeq.map({ 
        case (k, v) => "%s-%s".format(k, v) }).mkString("_")
    }

    experimentMaps.map(entryTitle).toIndexedSeq
  }  
  
  def apply(experiments: IndexedSeq[IndexedSeq[Experiment]]): Table[Experiment] = {
    // TODO: Replace with |everywhere| from shapeless when Scala 2.10 comes out.
    val experimentsFirstRow = experiments.head
    val experimentsFirstColumn = experiments.map(_.head)

    val tableTitle = title(experiments.flatten)
    val rowLabels = entryTitles(experimentsFirstColumn)
    val columnLabels = entryTitles(experimentsFirstRow)

    Table(tableTitle, rowLabels, columnLabels, experiments.toMatrix)
  }
}