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

import SummaryUtil._

///////////////////////////////////////////////////////////////////////////////

// TODO: These enumerated types probably aren't necessary since these won't
// be serialized with JSON.
//sealed trait ExperimentSummary
//
//case class ExperimentSummaryDouble(
//  name: String,
//  value: Double) extends ExperimentSummary
//
//case class ExperimentSummaryBufferedImage(
//  name: String,
//  value: BufferedImage) extends ExperimentSummary

///////////////////////////////////////////////////////////////////////////////  

// TODO: Change the table type names and remove the duplication.
// TODO: More general than WideBaseline
//case class Table(
//  title: String,
//  rowLabels: Seq[String],
//  columnLabels: Seq[String],
//  entries: Seq[Seq[Seq[ExperimentSummary]]])

case class Table[A](
  title: String,
  rowLabels: IndexedSeq[String],
  columnLabels: IndexedSeq[String],
  entries: DenseMatrix[A]) {
  lazy val unixEpoch = System.currentTimeMillis / 1000L
}

object Table {
  implicit def implicitFunctions[A](self: Table[A]) = new {
    def toTSV(toString: A => String): String = {
      val topRow = self.title +: self.columnLabels
      val stringEntries: Seq[Seq[String]] = self.entries.map(toString).toSeqSeq
      val otherRows: Seq[Seq[String]] = self.rowLabels.zip(stringEntries).map({
        case (title, entries) => Seq(title) ++ entries
      })
      val stringsTable = Seq(topRow) ++ otherRows

      stringsTable.map(_.mkString("\t")).mkString("\n")
    }

    def path: File = Global.run[RuntimeConfig].projectChildPathNew(
      "summary/%s_%s.csv".format(self.unixEpoch, self.title))
  }

  def title(experiments: Seq[Experiment]): String = {
    val maps = experiments.map(_.stringMap).toSet
    summarizeStructure(maps)
  }

  def entryTitles(experiments: Seq[Experiment]): IndexedSeq[String] = {
    val experimentMaps = experiments.map(_.stringMap)
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

//case class TableUnrendered(
//  title: String,
//  rowLabels: Seq[String],
//  columnLabels: Seq[String],
//  entries: Seq[Seq[WideBaselineExperiment]]) {
//  //  def toTSV(toString: WideBaselineExperiment => String): String = {
//  //    val topRow = Seq(title) ++ columnLabels
//  //    val stringEntries: Seq[Seq[String]] = entries.map(_.map(toString))
//  //    val otherRows: Seq[Seq[String]] = rowLabels.zip(stringEntries).map({
//  //      case (title, entries) => Seq(title) ++ entries
//  //    })
//  //    val stringsTable = Seq(topRow) ++ otherRows
//  //
//  //    stringsTable.map(_.mkString("\t")).mkString("\n")
//  //  }
//
//  //  def render(summarize: WideBaselineExperiment => Seq[ExperimentSummary]): Table = {
//  //    Table(
//  //      title,
//  //      rowLabels,
//  //      columnLabels,
//  //      entries.map(_.map(summarize)))
//  //  }
//
//  //  def toBag[A](convert: (String, String, WideBaselineExperiment) => A): Seq[A] = {
//  //    for (
//  //      (rowLabel, row) <- rowLabels.zip(entries);
//  //      (columnLabel, entry) <- columnLabels.zip(row)
//  //    ) yield {
//  //      convert(rowLabel, columnLabel, entry)
//  //    }
//  //  }
//
//}

object TableUnrendered {
  //  def recognitionRate: WideBaselineExperiment => Double = experiment =>
  //    SummaryUtil.recognitionRate(WideBaselineExperimentResults(experiment).dmatches)

}