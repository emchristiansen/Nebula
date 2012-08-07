package nebula

import java.io.File

case class RuntimeConfig(
  val projectRoot: File,
  val parallel: Boolean,
  val tempDirectory: String,
  val deleteTemporaryFiles: Boolean,
  val skipCompletedExperiments: Boolean,
  val maxSimultaneousExperiments: Int) {
  def childPath(child: String): File = {
    val path = new File(projectRoot, child)
    if (!path.exists) throw new Exception("Path does not exist: %s".format(path))
    path
  }
}

trait Experiment {
  val parameterAbbreviations: List[String]
  val parameterValues: List[String]

  val filenameNoTime: String = {
    assert(parameterAbbreviations.size == parameterValues.size)
    val parts = (parameterAbbreviations, parameterValues).zipped.map(_ + "-" + _)
    parts.mkString("_") + ".json"
  }

  val unixEpoch = System.currentTimeMillis / 1000L
  val filename: String = unixEpoch + "_" + filenameNoTime

  lazy val outDirectory: File = Global.run.childPath("results/experiment_data")

  lazy val path: File = {
    new File(outDirectory, filename)
  }

  def existingResultsFiles: List[File] = {
    val allPaths = outDirectory.list.toList.map(_.toString)
    val matchingPaths = allPaths.filter(_.contains(filenameNoTime))
    matchingPaths.sortBy(identity).reverse.map(path => new File(path))
  }

  def existingResultsFile: Option[File] = {
    existingResultsFiles match {
      case Nil => None
      case file :: _ => Some(file)
    }
  }

  def alreadyRun: Boolean = !existingResultsFile.isEmpty
}

