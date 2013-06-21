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
import nebula.util._

///////////////////////////////////////////////////////////

//case class Compressed[A](val bytes: Array[Byte])
//
//trait CompressedOps {
//  implicit class Decompress[A](compressed: Compressed[A]) {
//    def decompress
//  }
//}

case class CompressedString(bytes: Array[Byte])

trait CompressedStringOps {
  implicit class AddDecompress(self: CompressedString) {
    def decompress: String = {
      //      // Commons Compress doesn't seem to work well.
      //      val compressedFile = File.createTempFile("decompress", ".gz")
      //      FileUtils.writeByteArrayToFile(compressedFile, self.bytes)
      //      nebula.util.IO.runSystemCommand(s"gunzip ${compressedFile}", false)
      //      val uncompressedFile =
      //        new File(compressedFile.getPath.reverse.drop(3).reverse)
      //
      //      compressedFile.deleteOnExit
      //      uncompressedFile.deleteOnExit
      //
      //      FileUtils.readFileToString(uncompressedFile)

      val compressed = {
        val byteArrayInput = new ByteArrayInputStream(self.bytes)
        new BufferedInputStream(byteArrayInput)
      }

      val uncompressed =
        new CompressorStreamFactory().createCompressorInputStream(compressed)

      IOUtils.toString(uncompressed)
    }
  }
}

object CompressedString extends CompressedStringOps

////////////////////////////////////////////

/**
 * Holds files that exist and are actual files (not directories).
 */
case class ExistingFile(file: File) extends Box[File] {
  require(file.isFile, s"${file} is not an existing file.")
  
  override def get = file
}

object ExistingFile {
  def apply(filename: String): ExistingFile = ExistingFile(new File(filename))
}

/**
 * Holds directories that exist and are actual directories (not files).
 */
// TODO: Add method "get" to these wrapper classes.
case class ExistingDirectory(directory: File) extends Box[File] {
  require(directory.isDirectory, s"${directory} is not an existing directory.")
  
  override def get = directory
}

object ExistingDirectory {
  def apply(filename: String): ExistingDirectory =
    ExistingDirectory(new File(filename))
}

trait IO {
  val homeDirectory = new File(System.getProperty("user.home"))

  def getResource(path: String): ExistingFile =
    ExistingFile(getClass.getResource(path).getFile)

  implicit class AddCompress(self: String) {
    def compress: CompressedString = {
      //      // Commons Compress doesn't work well.
      //      
      //      val uncompressedFile = File.createTempFile("compress", "")
      //      FileUtils.writeStringToFile(uncompressedFile, self)
      //      nebula.util.IO.runSystemCommand(s"gzip ${uncompressedFile}", false)
      //      val compressedFile =
      //        new File(uncompressedFile.getPath + ".gz")
      //
      //      compressedFile.deleteOnExit
      //      uncompressedFile.deleteOnExit
      //
      //      CompressedString(FileUtils.readFileToByteArray(compressedFile))      

      val outputStream = new ByteArrayOutputStream

      val compressor =
        new CompressorStreamFactory().createCompressorOutputStream(
          CompressorStreamFactory.GZIP,
          outputStream)

      IOUtils.write(self, compressor)
      compressor.close

      CompressedString(outputStream.toByteArray)
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

    def compressedReadString: String = {
      asserty(file.toString.endsWith(".gz"))

      val bytes = FileUtils.readFileToByteArray(file)
      CompressedString(bytes).decompress

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

    def compressedWriteString(string: String) {
      asserty(file.toString.endsWith(".gz"))

      val CompressedString(bytes) = string.compress

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