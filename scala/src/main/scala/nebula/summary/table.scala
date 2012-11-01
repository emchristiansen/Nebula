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
case class Table(
  title: String,
  rowLabels: Seq[String],
  columnLabels: Seq[String],
  entries: Seq[Seq[Seq[ExperimentSummary]]])

case class TableUnrendered(
  title: String,
  rowLabels: Seq[String],
  columnLabels: Seq[String],
  entries: Seq[Seq[WideBaselineExperiment]]) {
  def toTSV(toString: WideBaselineExperiment => String): String = {
    val topRow = Seq(title) ++ columnLabels
    val stringEntries: Seq[Seq[String]] = entries.map(_.map(toString))
    val otherRows: Seq[Seq[String]] = rowLabels.zip(stringEntries).map({ case (title, entries) => Seq(title) ++ entries })
    val stringsTable = Seq(topRow) ++ otherRows

    stringsTable.map(_.mkString("\t")).mkString("\n")
  }

  def render(summarize: WideBaselineExperiment => Seq[ExperimentSummary]): Table = {
    Table(
      title,
      rowLabels,
      columnLabels,
      entries.map(_.map(summarize)))
  }

  def toBag[A](convert: (String, String, WideBaselineExperiment) => A): Seq[A] = {
    for (
      (rowLabel, row) <- rowLabels.zip(entries);
      (columnLabel, entry) <- columnLabels.zip(row)
    ) yield {
      convert(rowLabel, columnLabel, entry)
    }
  }

  lazy val unixEpoch = System.currentTimeMillis / 1000L
  def path: File = Global.run[RuntimeConfig].projectChildPathNew("summary/%s_%s.csv".format(unixEpoch, title))
}

object TableUnrendered {
  def recognitionRate: WideBaselineExperiment => Double = experiment =>
    SummaryUtil.recognitionRate(WideBaselineExperimentResults(experiment).dmatches)

  def apply(experiments: Seq[Seq[WideBaselineExperiment]]): TableUnrendered = {
    // TODO: Replace with |everywhere| from shapeless when Scala 2.10 comes out.
    val experimentsFirstRow = experiments.head
    val experimentsFirstColumn = experiments.map(_.head)

    val title = SummaryUtil.tableTitle(experiments.flatten)
    val rowLabels = SummaryUtil.tableEntryTitles(experimentsFirstColumn)
    val columnLabels = SummaryUtil.tableEntryTitles(experimentsFirstRow)

    TableUnrendered(title, rowLabels, columnLabels, experiments)
  }
}