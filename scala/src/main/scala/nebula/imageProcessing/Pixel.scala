package nebula.imageProcessing

import java.awt.{Color, Rectangle}
import java.awt.color.ColorSpace
import java.awt.geom.AffineTransform
import java.awt.image.{AffineTransformOp, BufferedImage, ColorConvertOp, ConvolveOp, DataBufferInt, Kernel}

import nebula.graveyard._
import nebula.mpie._
import nebula.summary._
import nebula.smallBaseline._
import nebula.util._
import nebula.util.imageProcessing.CIELab;
import nebula.util.imageProcessing.LinearRGBConverter;
import nebula.util.imageProcessing._
import nebula.wideBaseline._
import nebula._

import scala.Array.{canBuildFrom, fallbackCanBuildFrom}

import org.opencv.features2d.KeyPoint

///////////////////////////////////////////////////////////

case class Pixel(alpha: Int, red: Int, green: Int, blue: Int) {
  private def valid(color: Int): Boolean = color >= 0 && color <= 255
  
  assert(valid(alpha) && valid(red) && valid(green) && valid(blue))

  def isSimilar(threshold: Int, that: Pixel): Boolean = {
    (alpha - that.alpha).abs <= threshold &&
    (red - that.red).abs <= threshold &&
    (green - that.green).abs <= threshold &&
    (blue - that.blue).abs <= threshold
  }
  
  def argb: Int = {
    val a: Int = (alpha & 0xff) << 24
    val r: Int = (red & 0xff) << 16
    val g: Int = (green & 0xff) << 8
    val b: Int = (blue & 0xff) << 0

    a | r | g | b
  }

  def apply(index: Int): Int = index match {
    case 0 => alpha
    case 1 => red
    case 2 => green
    case 3 => blue
  }

  def gray: Seq[Int] = Seq((red + green + blue) / 3)
  def sRGB: Seq[Int] = Seq(red, green, blue)
  def lRGB: Seq[Int] = {
    val converter = new LinearRGBConverter
    converter.fromRGB(Array(red, green, blue).map(_.toFloat / 255)).map(x => (255 * x).toInt).toSeq
  }
  def hsb: Seq[Int] = {
    Color.RGBtoHSB(red, green, blue, null).map(x => (255 * x).toInt).toSeq
  }
  def lab: Seq[Int] = {
    val converter = CIELab.getInstance
    //    println(List(red, green, blue))
    // println(converter.fromRGB(Array(red, green, blue).map(_.toFloat)).toSeq)
    // println(converter.fromRGB(Array(red, green, blue).map(_.toFloat / 255)).toSeq)
    converter.fromRGB(Array(red, green, blue).map(_.toFloat / 255)).toSeq.map(_.toInt)
  }
  // def luv: Seq[Int] = {
  //   sys.error("Broken")
  //   val luvColorSpace = new LUVColorSpace
  //   println("here")
  //   println(luvColorSpace.fromRGB(Array(red, green, blue).map(_.toFloat)).toSeq)
  //   luvColorSpace.fromRGB(Array(red, green, blue).map(_.toFloat)).map(_.toInt).toSeq
  // }
  def xyz: Seq[Int] = {
    val sRGB = ColorSpace.getInstance(ColorSpace.CS_sRGB)
    sRGB.toCIEXYZ(Array(red, green, blue).map(_.toFloat)).map(x => (255 * x).toInt).toSeq
  }
}

object Pixel {
  def breakIntoBytes(int: Int): Tuple4[Int, Int, Int, Int] = {
    val byte0: Int = (int >> 24) & 0xff
    val byte1: Int = (int >> 16) & 0xff
    val byte2: Int = (int >> 8) & 0xff
    val byte3: Int = (int) & 0xff
    (byte0, byte1, byte2, byte3)
  }

  def getPixel(image: BufferedImage, x: Int, y: Int): Pixel = {
    val argb = image.getRGB(x, y)
    val (alpha, red, green, blue) = breakIntoBytes(argb)

    Pixel(alpha, red, green, blue)
  }

  // TODO: Rename
  def getPixelsOriginal(image: BufferedImage): IndexedSeq[Pixel] = {
    (for (
      h <- 0 until image.getHeight;
      w <- 0 until image.getWidth
    ) yield {
      getPixel(image, w, h)
    }).toIndexedSeq
  }

  def getPixels(image: BufferedImage): IndexedSeq[Int] = {
    (for (
      h <- 0 until image.getHeight;
      w <- 0 until image.getWidth
    ) yield {
      val Pixel(_, red, green, blue) = getPixel(image, w, h)
      List(red, green, blue)
    }).flatten.toIndexedSeq
  }

  def getPixelsGray(image: BufferedImage): IndexedSeq[Int] = {
    (for (
      h <- 0 until image.getHeight;
      w <- 0 until image.getWidth
    ) yield {
      getPixel(image, w, h).gray.head
    }).toIndexedSeq
  }

  def getPixelsColorSpace(colorSpace: ColorSpace, image: BufferedImage): IndexedSeq[Int] = {
    val colorConvertOp = new ColorConvertOp(colorSpace, null)
    val convertedImage = colorConvertOp.filter(image, null)

    val packedPixels = image.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData

    packedPixels.map(breakIntoBytes).flatMap({ case (a, b, c, d) => List(a, b, c, d) })
  }

  def fromUnclipped(a: Int, r: Int, g: Int, b: Int): Pixel = {
    Pixel(clip(a), clip(r), clip(g), clip(b))
  }

  def fromUnclipped(a: Int, r: Double, g: Double, b: Double): Pixel = {
    fromUnclipped(a, r.round.toInt, g.round.toInt, b.round.toInt)
  }

  private def clip(pixel: Int): Int = pixel.max(0).min(255)

  def add(p1: Pixel, p2: Pixel): Pixel = {
    assert(p1.alpha == p2.alpha)

    fromUnclipped(p1.alpha, p1.red + p2.red, p1.green + p2.green, p1.blue + p2.blue)
  }

  def add(p: Pixel, red: Int, green: Int, blue: Int): Pixel = {
    fromUnclipped(p.alpha, p.red + red, p.green + green, p.blue + blue)
  }

  def scale(p: Pixel, factor: Double): Pixel = {
    fromUnclipped(p.alpha, p.red * factor, p.green * factor, p.blue * factor)
  }

  def power(pixel: Pixel, pwr: Double): Pixel = {
    def single(v: Int): Int = {
      val normalized = v.toDouble / 255.0
      val out = math.pow(normalized, pwr)
      (out * 255).round.toInt
    }

    val Pixel(a, r, g, b) = pixel
    fromUnclipped(a, single(r), single(g), single(b))
  }

  def green: Int = Pixel(255, 0, 255, 0).argb
}