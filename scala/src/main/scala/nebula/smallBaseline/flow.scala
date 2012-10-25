package nebula.smallBaseline

import java.awt.image.BufferedImage
import java.io.File
import org.apache.commons.io.FileUtils.readFileToString
import breeze.linalg.DenseMatrix
import grizzled.math.stats
import javax.imageio.ImageIO
import nebula.graveyard._
import nebula.mpie._
import nebula.summary._
import nebula.smallBaseline._
import nebula.util._
import nebula.util.imageProcessing._
import nebula.wideBaseline._
import nebula._

///////////////////////////////////////////////////////////

// TODO: Implement with breeze vector and implicits, name Vector2. 
case class FlowVector(horizontal: Double, vertical: Double)

object FlowVector {
  implicit def addL2Distance(self: FlowVector) = new {
    def l2Distance(that: FlowVector): Double = {
      val dX = self.horizontal - that.horizontal
      val dY = self.vertical - that.vertical
      math.sqrt(math.pow(dX, 2) + math.pow(dY, 2))
    }
  }
}

case class FlowField(data: DenseMatrix[Option[FlowVector]])

///////////////////////////////////////////////////////////

object FlowField {
  def apply(file: File): FlowField = {
    require(file.getName.endsWith(".flo.txt"))

    val contents = readFileToString(file).split("\n").filter(_.size > 0).toSeq

    val (width, height, depth) = {
      val HeaderRegex = """(\d+) (\d+) (\d+)""".r
      val HeaderRegex(width, height, depth) = contents.head
      (width.toInt, height.toInt, depth.toInt)
    }

    case class Line(y: Int, x: Int, channel: Int, value: Option[Double])

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
  // TODO: Make name accurate.
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