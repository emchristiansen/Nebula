package nebula

import java.io.File
import spray.json._
import org.apache.commons.io.FileUtils
import nebula.util._

///////////////////////////////////////////////////////////

trait ExperimentRunner[R] {
  def run: R
}

trait StorageInfo[R] {
  def currentPath: File
  def mostRecentPath: Option[File]
  def save: R => Unit
  def load: Option[R]
  def name: String
}

object StorageInfo {
  implicit class Experiment2StorageInfoWithRuntimeConfig[E <% ExperimentRunner[R]: JsonFormat, R: JsonFormat](
    experiment: E)(
      implicit runtimeConfig: RuntimeConfig) extends StorageInfo[R] {
    override def currentPath: File = new File(outDirectory, filename)

    override def mostRecentPath: Option[File] = existingResultsFiles.headOption

    override def save = results => {
      println(s"Saving to ${currentPath}")
      val json = results.toJson
      org.apache.commons.io.FileUtils.writeStringToFile(currentPath, json.prettyPrint)
    }

    override def load = mostRecentPath map { file =>
      println(s"Loading ${file}")
      val jsonString = FileUtils.readFileToString(file)
      jsonString.asJson.convertTo[R]
    }

    override def name = unixEpoch + "_" + nameNoTime

    ///////////////////////////////////////////////////////

    val unixEpoch = System.currentTimeMillis / 1000L

    def nameNoTime: String = {
      val fullString = JSONUtil.flattenJson(experiment.toJson)
      //      // Unfortunately, this string is too long to be part of a filename.
      //      fullString.take(100) + "_" + fullString.hashCode
      fullString
    }

    def filenameNoTime: String = nameNoTime + ".json"

    def filename: String = unixEpoch + "_" + filenameNoTime

    def outDirectory: File = runtimeConfig.projectChildPath(
      "results/experiment_data/")

    def existingResultsFiles: Seq[File] = {
      val allPaths = outDirectory.list.toList.map(path => outDirectory + "/" + path.toString)
      val matchingPaths = allPaths.filter(_.contains(filenameNoTime))
      matchingPaths.sortBy(identity).reverse.map(path => new File(path))
    }
  }

  implicit def experiment2StorageInfo[E <% ExperimentRunner[R]: JsonFormat, R: JsonFormat](
    experiment: E): RuntimeConfig => StorageInfo[R] = runtimeConfig => {
    implicit val iRC = runtimeConfig
    experiment
  }
}



