package nebula

import scala.reflect.runtime._
import scala.reflect._
import java.io.File
import spray.json._
import org.apache.commons.io.FileUtils
import java.io.FileInputStream
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream
import java.io.FileOutputStream
import java.io.BufferedInputStream
import org.apache.commons.compress.compressors.CompressorStreamFactory
import org.apache.commons.io.IOUtils
import java.io.OutputStream
import java.io.ByteArrayOutputStream
import java.io.FileInputStream
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream
import java.io.FileOutputStream
import java.io.BufferedInputStream
import org.apache.commons.compress.compressors.CompressorStreamFactory
import org.apache.commons.io.IOUtils
import java.io.OutputStream
import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream

///////////////////////////////////////////////////////////

//case class Compressed[A](val bytes: Array[Byte])
//
//trait CompressedOps {
//  implicit class Decompress[A](compressed: Compressed[A]) {
//    def decompress
//  }
//}

case class BZ2String(bz2: Array[Byte])

trait BZ2StringOps {
  implicit class AddDecompress(self: BZ2String) {
    def decompress: String = {     
      val compressed = {
        val byteArrayInput = new ByteArrayInputStream(self.bz2)
        new BufferedInputStream(byteArrayInput)
      }

      val uncompressed =
        new CompressorStreamFactory().createCompressorInputStream(compressed)

      IOUtils.toString(uncompressed)      
    }
  }
}

object BZ2String extends BZ2StringOps

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

  implicit class AddCompress(self: String) {
    def compress: BZ2String = {
      val outputStream = new ByteArrayOutputStream
      
      val compressor =
        new CompressorStreamFactory().createCompressorOutputStream(
          CompressorStreamFactory.BZIP2,
          outputStream)

      IOUtils.write(self, compressor)
      compressor.close      
      
      BZ2String(outputStream.toByteArray)
    }
  }

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

    def readString: String = FileUtils.readFileToString(file)

    def writeString(string: String): Unit =
      FileUtils.writeStringToFile(file, string)

    def bz2ReadString: String = {
      asserty(file.toString.endsWith(".bz2"))

      val bytes = FileUtils.readFileToByteArray(file)
      BZ2String(bytes).decompress
      
//      val compressed = {
//        val fis = new FileInputStream(file)
//        new BufferedInputStream(fis)
//      }
//
//      val uncompressed =
//        new CompressorStreamFactory().createCompressorInputStream(compressed)
//
//      IOUtils.toString(uncompressed)
    }

    def bz2WriteString(string: String) {
      asserty(file.toString.endsWith(".bz2"))

      val BZ2String(bytes) = string.compress
      
      FileUtils.writeByteArrayToFile(file, bytes)
      
//      val outputStream = new FileOutputStream(file)
//
//      val compressor =
//        new CompressorStreamFactory().createCompressorOutputStream(
//          CompressorStreamFactory.BZIP2,
//          outputStream)
//
//      IOUtils.write(string, compressor)
//      compressor.close
    }
  }
}

object IO extends IO