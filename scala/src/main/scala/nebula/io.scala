package nebula
import scala.reflect.runtime._
import scala.reflect._
import java.io.File
import spray.json._

///////////////////////////////////////////////////////////

/**
 * Holds files that exist and are actual files (not directories).
 */
case class ExistingFile(file: File) {
  require(file.isFile, s"${file} is not an existing file.")
}

object ExistingFile {
  def apply(filename: String): ExistingFile = ExistingFile(new File(filename))

  implicit def existingFile2File(self: ExistingFile) =
    self.file
}

/**
 * Holds directories that exist and are actual directories (not files).
 */
case class ExistingDirectory(directory: File) {
  require(directory.isFile, s"${directory} is not an existing directory.")
}

object ExistingDirectory {
  def apply(filename: String): ExistingDirectory =
    ExistingDirectory(new File(filename))

  implicit def existingDirectory2File(self: ExistingDirectory) =
    self.directory
}

trait IO {
  val homeDirectory = new File(System.getProperty("user.home"))

  def getResource(path: String): ExistingFile =
    ExistingFile(getClass.getResource(path).getFile)

  implicit class PimpFile(file: File) {
    def mustExist: File = {
      assert(file.exists, s"File ${file} doesn't exist")
      file
    }

    def parentMustExist: File = {
      new File(file.getParent).mustExist
      file
    }

    def +(that: String): File = new File(file, that)
  }
}

object IO extends IO