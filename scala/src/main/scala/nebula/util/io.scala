package nebula.util

import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import scala.xml.Node
import scala.xml.XML
import com.twitter.util.Eval
import javax.imageio.ImageIO
//import net.liftweb.json.Formats
//import net.liftweb.json.Serialization
//import net.liftweb.json.Serialization.read
//import net.liftweb.json.Serialization.write
//import net.liftweb.json.ShortTypeHints
//import net.liftweb.json.parse
//import net.liftweb.json.pretty
//import net.liftweb.json.render
import nebula.Detector
import nebula.Extractor
import nebula.Matcher
import nebula.RuntimeConfig
import nebula.wideBaseline.WideBaselineExperiment
import nebula.wideBaseline.WideBaselineExperimentResults


import nebula.graveyard._
import nebula.mpie._
import nebula.summary._
import nebula.smallBaseline._
import nebula.util._
import nebula.util.imageProcessing._
import nebula.wideBaseline._
import nebula._

///////////////////////////////////////////////////////////

object ExperimentIO {
//  val extraSerializers = List(new DMatchSerializer)
//
//  val formats = Serialization.formats(ShortTypeHints(instances.toList)) ++ extraSerializers
}

///////////////////////////////////////////////////////////

object AtomicIO {
  val lockPath = "/tmp/eric_demo_lock"

  private def getLock(stream: FileOutputStream): java.nio.channels.FileLock = {
    try {
      stream.getChannel.lock()
    } catch {
      case e: java.nio.channels.OverlappingFileLockException => {
        Thread.sleep(50)
        getLock(stream)
      }
    }
  }

  def readImage(file: File): BufferedImage = {
    val foStream = new FileOutputStream(new File(lockPath))
    val lock = getLock(foStream)

    val image = ImageIO.read(file)

    lock.release()
    foStream.close()
    image
  }

  def writeImage(image: BufferedImage, formatName: String, file: File) {
    val foStream = new FileOutputStream(new File(lockPath))
    val lock = getLock(foStream)

    ImageIO.write(image, formatName, file)

    lock.release()
    foStream.close()
  }
}

///////////////////////////////////////////////////////////

object IO {
  def interpretFile[A : Manifest](file: File): A = {
    val source = org.apache.commons.io.FileUtils.readFileToString(file)
    eval[A](source)
  }

  def dumpSourceToFile[A](obj: A, file: File) {
    org.apache.commons.io.FileUtils.writeStringToFile(file, obj.toString)
  }
  
  def objectToByteArray[A](obj: A): Array[Byte] = {
    val byte_stream = new ByteArrayOutputStream
    (new ObjectOutputStream(byte_stream)).writeObject(obj)
    byte_stream.toByteArray
  }

  def byteArrayToObject[A](byte_array: Array[Byte])(implicit m: Manifest[A]): A = {
    val obj = new ObjectInputStream(new ByteArrayInputStream(byte_array)) readObject

    obj match {
      case a if m.erasure.isInstance(a) => a.asInstanceOf[A]
      case _ => { sys.error("Type not what was expected when reading from file") }
    }
  }

  def writeObjectToFile[A](obj: A, file: File) {
    val output = new ObjectOutputStream(new FileOutputStream(file, false))
    output.writeObject(obj)
    output.close()
  }

  def writeObjectToFilename[A](obj: A, filename: String) = writeObjectToFile(obj, new File(filename))

  def readObjectFromFile[A](file: File)(implicit m: Manifest[A]): A = {
    val obj = new ObjectInputStream(new FileInputStream(file)) readObject

    obj match {
      case a if m.erasure.isInstance(a) => a.asInstanceOf[A]
      case _ => { sys.error("Type not what was expected when reading from file") }
    }
  }

  def readObjectFromFilename[A](filename: String)(implicit m: Manifest[A]) = readObjectFromFile[A](new File(filename))

//  def fromJSONFileAbstract[A <: AnyRef](formats: Formats, file: File)(implicit manifest: Manifest[A]): A = {
//    val json = org.apache.commons.io.FileUtils.readFileToString(file)
//    // TODO: There must be a better way to make |formats| implicit.
//    implicit val implicitFormats = formats
//    read[A](json)
//  }
//
//  def fromJSONFile[A <: AnyRef](file: File)(implicit manifest: Manifest[A]): A = {
//    val formats = Serialization.formats(ShortTypeHints(List(manifest.erasure)))
//    fromJSONFileAbstract[A](formats, file)
//  }
//
//  def toJSONFileAbstract[A <: AnyRef](formats: Formats, item: A, file: File)(implicit manifest: Manifest[A]) {
//    // TODO: There must be a better way to make |formats| implicit.
//    implicit val implicitFormats = formats
//    val json = pretty(render(parse(write(item))))
//    org.apache.commons.io.FileUtils.writeStringToFile(file, json)
//  }
//
//  def toJSONFile[A <: AnyRef](item: A, file: File)(implicit manifest: Manifest[A]) {
//    val formats = Serialization.formats(ShortTypeHints(List(manifest.erasure)))
//    toJSONFileAbstract(formats, item, file)
//  }

  def writeImage(directory: File, image: BufferedImage): File = {
    require(directory.isDirectory)
    val file = File.createTempFile("histogram", ".png", directory)
    ImageIO.write(image, "png", file)
    file
  }

  def runSystemCommand(command: String): String = {
    println("running system command: %s".format(command))
    try {
      val out = sys.process.Process(command).!!
      println("successfully ran system command")
      out
    } catch {
      case e: Exception => throw new Exception("system command failed: %s\nException was %s\n".format(command, e.toString))
    }
  }

  def runSystemCommandWaitFile(executable: String, path: String) {
    println("running system command: %s".format(executable))
    java.lang.Runtime.getRuntime.exec(executable)
    println("waiting for file: %s".format(path))
    while (!(new File(path).exists)) Thread.sleep(100) // 100 milliseconds
  }

  def createTempFile(prefix: String, suffix: String) = {
    val file = Global.run[RuntimeConfig].tempDirectory match {
      case Some(file) => File.createTempFile(prefix, suffix, file)
      case None => File.createTempFile(prefix, suffix)
    }

    if (Global.run[RuntimeConfig].deleteTemporaryFiles) file.deleteOnExit
    file
  }
}
