package nebula

import java.io.File

trait RuntimeConfigTrait {
  val projectRoot: String
  val parallel: Boolean
  val tempDirectory: String
  val deleteTemporaryFiles: Boolean
  val skipCompletedExperiments: Boolean
  val maxSimultaneousExperiments: Int
  def childPath(child: String): File = {
    val path = new File(projectRoot, child)
    if (!path.exists) throw new Exception("Path does not exist: %s".format(path))
    path
  }
}

// TODO: This duplication is a bit janky, but I don't want to use anything other
// than case classes and traits, and given that constraint don't see a way to
// avoid this.
case class RuntimeConfig (
  override val projectRoot: String,
  override val parallel: Boolean,
  override val tempDirectory: String,
  override val deleteTemporaryFiles: Boolean,
  override val skipCompletedExperiments: Boolean,
  override val maxSimultaneousExperiments: Int) extends RuntimeConfigTrait

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

  lazy val outDirectory: File = Global.run[RuntimeConfig].childPath("results/experiment_data")

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

