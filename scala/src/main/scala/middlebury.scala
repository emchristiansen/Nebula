package nebula

import breeze.linalg._
import java.io.File
import org.apache.commons.io.FileUtils._
import java.awt.image.BufferedImage

import javax.imageio.ImageIO

import grizzled.math._

case class FlowVector(val horizontal: Double, val vertical: Double)

object FlowVector {
  implicit def addL2Distance(self: FlowVector) = new {
    def l2Distance(that: FlowVector): Double = {
      val dX = self.horizontal - that.horizontal
      val dY = self.vertical - that.vertical
      math.sqrt(math.pow(dX, 2) + math.pow(dY, 2))
    }
  }
}

case class FlowField(val data: DenseMatrix[Option[FlowVector]])

object FlowField {
  def fromFloFile(file: File): FlowField = {
    require(file.getName.endsWith(".flo.txt"))

    val contents = readFileToString(file).split("\n").filter(_.size > 0).toSeq

    val (width, height, depth) = {
      val HeaderRegex = """(\d+) (\d+) (\d+)""".r
      val HeaderRegex(width, height, depth) = contents.head
      (width.toInt, height.toInt, depth.toInt)
    }

    case class Line(val y: Int, val x: Int, val channel: Int, val value: Option[Double])

    val lines = for (lineString <- contents.tail) yield {
      val LineRegex = """(\d+) (\d+) (\d+) (.+)""".r
      val LineRegex(y, x, channel, value) = lineString
      assert(channel.toInt == 0 || channel.toInt == 1)
      Line(
        y.toInt,
        x.toInt,
        channel.toInt,
        if (value.toDouble.abs < 1000) Some(value.toDouble) else None)
    }

    val groups = {
      val groups = lines.groupBy(line => line.y + "_" + line.x).values.toSeq
      groups.map(_.sortBy(_.channel))
    }
    assert(groups.size == lines.size / 2)
    assert(groups.forall(_.size == 2))

    val data = DenseMatrix.fill[Option[FlowVector]](height, width)(None)
    for (Seq(channel0, channel1) <- groups; if channel0.value.isDefined && channel1.value.isDefined) {
      val entry = Some(FlowVector(channel0.value.get, channel1.value.get))
      data(channel0.y, channel0.x) = entry
    }

    FlowField(data)
  }

  implicit def implicitDenseMatrix(self: FlowField): DenseMatrix[Option[FlowVector]] =
    self.data

  // Actually mean l2 distance.
  implicit def addL2Distance(self: FlowField) = new {
    def l2Distance(that: FlowField): Double = {
      require(self.data.rows == that.data.rows)
      require(self.data.cols == that.data.cols)

      val thisIterator = self.data.activeValuesIterator
      val thatIterator = that.data.activeValuesIterator

      val distances = for ((Some(left), Some(right)) <- thisIterator.zip(thatIterator).toSeq) yield {
        left.l2Distance(right)
      }

//      math.sqrt(distances.map(d => math.pow(d, 2)).sum)
      stats.mean(distances: _*)
    }
  }
}

case class SmallBaselinePair(left: BufferedImage, right: BufferedImage, flow: FlowField) {
  require(left.getWidth == right.getWidth)
  require(left.getWidth == flow.data.cols)
  require(left.getHeight == right.getHeight)
  require(left.getHeight == flow.data.rows)
}

object SmallBaselinePair {
  def fromName(directoryRoot: File, name: String): SmallBaselinePair = {
    require(directoryRoot.isDirectory)

    def getFile(format: String): File = {
      val filename = format.format(name)
      val file = new File(directoryRoot, filename)
      assert(file.isFile, "not a file: %s".format(file.toString))
      file
    }

    val flow = {
      val file = getFile("/other-gt-flow/%s/flow10.flo.txt")
      FlowField.fromFloFile(file)
    }

    def getImage(format: String): BufferedImage = {
      val file = getFile(format)
      val image = ImageIO.read(file)
      assert(image != null)
      image
    }

    val left = getImage("/other-data/%s/frame10.png")
    val right = getImage("/other-data/%s/frame11.png")

    SmallBaselinePair(left, right, flow)
  }
}