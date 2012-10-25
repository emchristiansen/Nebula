package nebula.summary

import nebula.graveyard._
import nebula.mpie._
import nebula.summary._
import nebula.smallBaseline._
import nebula.util._
import nebula.util.imageProcessing._
import nebula.wideBaseline._
import nebula._

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import org.opencv.features2d.DMatch

///////////////////////////////////////////////////////////////////////////////

case class Histogram(
  title: String,
  sameDistances: Seq[Double],
  differentDistances: Seq[Double]) {
  def draw {
    val image = render
    ImageIO.write(image, "png", path)

    println("wrote %s".format(path))
  }

  // TODO: Improve this and the above name.
  def render: BufferedImage = {
    val tempContents = "%s\n%s\n%s".format(
      title,
      sameDistances.sorted.mkString(" "),
      differentDistances.sorted.mkString(" "))
    val tempFile = IO.createTempFile("histogramData", ".txt")
    org.apache.commons.io.FileUtils.writeStringToFile(tempFile, tempContents)

    // TODO: Fix path
    val pythonScript = Global.run[RuntimeConfig].nebulaChildPath("python/distance_histogram.py")
    val outputFile = IO.createTempFile("histogram", ".png")
    val command = "python %s %s %s".format(pythonScript, tempFile, outputFile)
    IO.runSystemCommand(command)

    ImageIO.read(outputFile)
  }

  def path: File = {
    val filename = title.replace(" ", "_") + ".png"
    Global.run[RuntimeConfig].projectChildPathNew("summary/histograms/%s".format(filename))
  }
}

object Histogram {
  def apply(experiment: WideBaselineExperiment, title: String): Histogram = {
    val results = WideBaselineExperimentResults(experiment)
    val (same, different) = results.dmatches.partition(dmatch => dmatch.queryIdx == dmatch.trainIdx)
    Histogram(title, same.map(_.distance.toDouble), different.map(_.distance.toDouble))
  }
}