package nebula

import java.io.File

trait RuntimeConfigTrait {
  val projectRoot: File
  val nebulaRoot: File
  val parallel: Boolean
  val tempDirectory: Option[File]
  val deleteTemporaryFiles: Boolean
  val skipCompletedExperiments: Boolean
  val maxSimultaneousExperiments: Int
  def projectChildPath(child: String): File = {
    val path = projectChildPathNew(child)
    if (!path.exists) throw new Exception("Path does not exist: %s".format(path))
    path
  }
  def projectChildPathNew(child: String): File = {
    new File(projectRoot, child)
  }
  def nebulaChildPath(child: String): File = {
    val path = new File(nebulaRoot, child)
    if (!path.exists) throw new Exception("Path does not exist: %s".format(path))
    path
  }
}

// TODO: This duplication is a bit janky, but I don't want to use anything other
// than case classes and traits, and given that constraint don't see a way to
// avoid this.
case class RuntimeConfig(
  override val projectRoot: File,
  override val nebulaRoot: File,
  override val parallel: Boolean,
  override val tempDirectory: Option[File],
  override val deleteTemporaryFiles: Boolean,
  override val skipCompletedExperiments: Boolean,
  override val maxSimultaneousExperiments: Int) extends RuntimeConfigTrait {
  require(projectRoot.isDirectory)
  for (temp <- tempDirectory) {
    if (!temp.isDirectory) assert(temp.mkdir)
  }
}

object RuntimeConfig {
  // TODO: Put this somewhere more appropriate.
  System.loadLibrary("opencv_java")

  def init(runtimeConfigFile: File) {
    val runtimeConfig = IO.interpretFile[RuntimeConfig](runtimeConfigFile)
    Global.runVar = Some(runtimeConfig)
    println(runtimeConfig)
  }
}

trait Experiment {
  def name: String
  // Parameter names and values
  def parameters: Seq[Tuple2[String, String]]

  type ResultsType 
  def run: ResultsType
  
  ///////////////////////////////////////////////////////////

  val unixEpoch = System.currentTimeMillis / 1000L

  def filenameNoTime: String =
    name + "_" + parameters.map(p => p._1 + "-" + p._2).mkString("_") + ".json"

  def filename: String = unixEpoch + "_" + filenameNoTime

  def outDirectory: File = Global.run[RuntimeConfig].projectChildPath("results/experiment_data")

  def path: File = new File(outDirectory, filename)

  def existingResultsFiles: Seq[File] = {
    val allPaths = outDirectory.list.toList.map(path => outDirectory + "/" + path.toString)
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
