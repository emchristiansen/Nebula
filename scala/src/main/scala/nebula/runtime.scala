package nebula

import java.io.File

import scala.util.Random

import util.IO

import spark.SparkContext
import SparkContext._

///////////////////////////////////////////////////////////

object Global {
  val random = new Random(0)
  val homeDirectory = new File(System.getProperty("user.home"))
  var runVar: Option[RuntimeConfigTrait] = None
  // TODO: This asInstanceOf stuff has bad code smell.
  def run[R <: RuntimeConfigTrait](implicit manifest: Manifest[R]): R = runVar match {
    case Some(config) => {
      assert(manifest.erasure.isInstance(config))
      config.asInstanceOf[R]
    }
    case None => throw new Exception("Global.run not initialized")
  }
}

///////////////////////////////////////////////////////////

trait RuntimeConfigTrait {
  val projectRoot: File
  val nebulaRoot: File
  val sparkContext: SparkContext
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
  override val sparkContext: SparkContext,
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



