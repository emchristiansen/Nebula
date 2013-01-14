package nebula

import java.io.File

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



