import java.io.File

import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Prop.{ forAll, propBoolean }
import org.scalacheck.Properties

import javax.imageio.ImageIO

import nebula.util.imageProcessing.ImageUtil._

import nebula.util.imageProcessing._
import nebula.util.imageProcessing.RichImage._

object CheckRichImage extends Properties("RichImage") {
  val url = getClass.getResource("/goldfish_girl.jpg")
  val image = ImageIO.read(new File(url.getFile))

  def monotonicity(x: Double, y: Double, channel: Int): Boolean = {
    val upperLeft = image.getSubPixel(x.floor, y.floor).get
    val upperRight = image.getSubPixel(x.ceil, y.floor).get
    val lowerLeft = image.getSubPixel(x.floor, y.ceil).get

    // TODO: The following two code blocks are nearly identical.
    val xPixel = image.getSubPixel(x, y.floor).get
    val xMonotonic = if (xPixel(channel) > upperLeft(channel)) {
      upperRight(channel) >= xPixel(channel)
    } else if (xPixel(channel) < upperLeft(channel)) {
      upperRight(channel) <= xPixel(channel)
    } else true

    val yPixel = image.getSubPixel(x.floor, y).get
    val yMonotonic = if (yPixel(channel) > upperLeft(channel)) {
      lowerLeft(channel) >= yPixel(channel)
    } else if (yPixel(channel) < upperLeft(channel)) {
      lowerLeft(channel) <= yPixel(channel)
    } else true

    xMonotonic && yMonotonic
  }

  implicit lazy val arbitraryXYChannel: Arbitrary[Tuple3[Double, Double, Int]] = {
    val xYChannel = for (
      x <- Gen.choose(0.0, image.getWidth - 1.0);
      y <- Gen.choose(0.0, image.getHeight - 1.0);
      channel <- Gen.choose(0, 3)
    ) yield (x, y, channel)
    Arbitrary(xYChannel)
  }

  property("monotonicity") = forAll {
    xYChannel: Tuple3[Double, Double, Int] =>
      {
        val (x, y, channel) = xYChannel
        monotonicity(x, y, channel)
      }
  }
}