package nebula

import java.io.File

import nebula.smallBaseline.SmallBaselineExperimentResults.implicitExperimentResults
import nebula.wideBaseline.WideBaselineExperimentResults.implicitExperimentResults
import smallBaseline.{SmallBaselineExperiment, SmallBaselineExperimentResults}
import wideBaseline.{WideBaselineExperiment, WideBaselineExperimentResults}

///////////////////////////////////////////////////////////

trait ExperimentResults {
  def experiment: Experiment
  def save: Unit

  def filenameNoTime: String =
    experiment.name + "_" + 
    experiment.parameters.map(p => p._1 + "-" + p._2).mkString("_") + 
    ".json"

  def filename: String = experiment.unixEpoch + "_" + filenameNoTime

  def outDirectory: File = Global.run[RuntimeConfig].projectChildPath("results/experiment_data")

  def path: File = new File(outDirectory, filename)

  def existingResultsFiles: Seq[File] = {
    val allPaths = outDirectory.list.toList.map(path => outDirectory + "/" + path.toString)
    val matchingPaths = allPaths.filter(_.contains(filenameNoTime))
    matchingPaths.sortBy(identity).reverse.map(path => new File(path))
  }

  def existingResultsFile: Option[File] = existingResultsFiles.headOption

  def alreadyRun: Boolean = !existingResultsFile.isEmpty
}

object ExperimentResults {
  def apply(experiment: Experiment): ExperimentResults = experiment.original match {
    case original: WideBaselineExperiment => WideBaselineExperimentResults(original)
    case original: SmallBaselineExperiment => SmallBaselineExperimentResults(original)
  }
}





