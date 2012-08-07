package nebula

import java.awt.Rectangle
import java.awt.geom._
import java.awt.image._
import java.awt.image.AffineTransformOp._
import java.io.File
import javax.imageio.ImageIO

import com.googlecode.javacv.cpp.opencv_core._
import com.googlecode.javacv.cpp.opencv_highgui._
import com.googlecode.javacv.cpp.opencv_features2d._

object OpenCVUtil {
  def bufferedImageToCvMat(image: BufferedImage): CvMat = {
    // TODO: Figure out how to do this without IO.
    val file = IO.createTempFile("bufferedImageToCvMat", ".bmp")
    ImageIO.write(image, "bmp", file)
    val matImage = cvLoadImageM(file.toString)
    assert(matImage != null)
    matImage
  }
}

object ImageProcessing {
  def boxBlur(boxWidth: Int, image: BufferedImage): BufferedImage = {
    val kernel = {
      val numPixels = boxWidth * boxWidth
      val kernelValues = Array.fill(numPixels)((1.0 / numPixels).toFloat)
      new Kernel(boxWidth, boxWidth, kernelValues)
    }
    val op = new ConvolveOp(kernel, ConvolveOp.EDGE_ZERO_FILL, null)
    op.filter(image, null)
  }

  // A subpixel version of |BufferedImage.getSubimage|.
  def getSubimage(image: BufferedImage,
		  x: Double, 
		  y: Double,
		  width: Int, 
		  height: Int): BufferedImage = {
    require(x >= 0)
    require(x + width < image.getWidth)
    require(y >= 0)
    require(y + height < image.getHeight)

    // Crop out enough of the image to just contain the patch.
    val xInt = x.floor.toInt
    val yInt = y.floor.toInt
    val boundingPatch = image.getSubimage(xInt, yInt, width + 1, height + 1)

    // Translate the patch, using subpixel interpolation, so the pixels
    // line up.
    val xResidual = x - xInt
    val yResidual = y - yInt
    
    val translation = new AffineTransform
    translation.translate(-xResidual, -yResidual)
    val op = new AffineTransformOp(translation, AffineTransformOp.TYPE_BILINEAR)
    val alignedPatch = op.filter(boundingPatch, null)

    // Extract the aligned patch.
    alignedPatch.getSubimage(0, 0, width, height)
  }

  def extractPatch(image: BufferedImage,
  		   patchWidth: Int,
  		   keyPoint: KeyPoint): Option[BufferedImage] = {
    try {
      val x = keyPoint.pt_x - patchWidth / 2.0
      val y = keyPoint.pt_y - patchWidth / 2.0
      Some(getSubimage(image, x, y, patchWidth, patchWidth))
    } catch {
      case e: IllegalArgumentException => None
    }
  }
}

case class Pixel(val alpha: Int, val red: Int, val green: Int, val blue: Int) { 
  private def valid(color: Int): Boolean = color >= 0 && color <= 255

  assert(valid(alpha) && valid(red) && valid(green) && valid(blue))

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

  def gray: Int = (red + green + blue) / 3
}

object Pixel { 
  def getPixel(image: BufferedImage, x: Int, y: Int): Pixel = { 
    val argb = image.getRGB(x, y)
    val alpha: Int = (argb >> 24) & 0xff
    val red: Int = (argb >> 16) & 0xff
    val green: Int = (argb >> 8) & 0xff
    val blue: Int = (argb) & 0xff

    Pixel(alpha, red, green, blue)
  }

  def getPixels(image: BufferedImage): List[Int] = {
    (for (h <- 0 until image.getHeight;
	  w <- 0 until image.getWidth) yield { 
      val Pixel(_, red, green, blue) = getPixel(image, w, h)
      List(red, green, blue)
    }).flatten.toList
  }

  def getPixelsGray(image: BufferedImage): List[Int] = {
    (for (h <- 0 until image.getHeight;
	  w <- 0 until image.getWidth) yield { 
      getPixel(image, w, h).gray
    }).toList
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

object Image {
  def deepCopy(bi: BufferedImage): BufferedImage = {
    val newImage = new BufferedImage(bi.getWidth, bi.getHeight, bi.getType)
    val graphics = newImage.createGraphics
    graphics.drawImage(bi, null, 0, 0)
    newImage
  }

  def extractROI(roiImg: BufferedImage, image: BufferedImage): BufferedImage = {
    // after loading up the roi, figure out the crop window size.
    //val hasAlphaChannel = image.getAlphaRaster() != null
    val cols = roiImg.getWidth()
    val rows = roiImg.getHeight()
    var minx = cols + 1;
    var miny = rows + 1;
    var maxx = -1;
    var maxy = -1;
    val maskedImg = new BufferedImage(cols, rows, image.getType())
    for(i_rows <- 0 until rows) {
      for(i_cols <- 0 until cols) {
	// if this pixel is not 100% black, then this is considered
	// to be an on pixel
	if( (roiImg.getRGB(i_cols, i_rows) & 0x00ffffff) != 0 ) {
	  // copy the pixel color value from original image to the masked
	  // image
	  maskedImg.setRGB(i_cols, i_rows, image.getRGB(i_cols, i_rows))

	  // see if this is a new minx, miny
	  if( i_cols < minx ) {
            minx = i_cols
	  }
	  if( i_cols > maxx ) {
            maxx = i_cols
	  }
	  if( i_rows < miny ) {
            miny = i_rows
	  }
	  if( i_rows > maxy ) {
            maxy = i_rows
	  }
	} // is this an on pixel?
	  else {
	    maskedImg.setRGB(i_cols,i_rows, Pixel.green)
	  } // else of is this an on pixel?

      } // loop over cols
    } // loop over rows
    val width = maxx - minx
    val height = maxy - miny

    // crop the image
    val crop_rect = new Rectangle(minx, miny, width, height)
    
    val overlap = crop_rect.intersection(new Rectangle(image.getWidth(), image.getHeight()))
    val clipped = maskedImg.getSubimage(overlap.x, overlap.y, overlap.width, overlap.height)

    clipped
  }
  
  def transparentToGreen(image: BufferedImage): BufferedImage = {
    for (y <- 0 until image.getHeight;
	 x <- 0 until image.getWidth) {
      val pixel = Pixel.getPixel(image, x, y)
      if (pixel.alpha == 0) image.setRGB(x, y, Pixel.green)
    }
    image
  }

  /* A common value according to http://en.wikipedia.org/wiki/Gamma_compression */
  private val gamma = 2.2

  private def powerImage(in: BufferedImage, pwr: Double): BufferedImage = {
    val out = new BufferedImage(in.getWidth, in.getHeight, in.getType)
    for (y <- 0 until out.getHeight;
	 x <- 0 until out.getWidth) {
      val pixel = Pixel.power(Pixel.getPixel(in, x, y), pwr)
      out.setRGB(x, y, pixel.argb)
    }
    out
  }

  def toRaw(image: BufferedImage): BufferedImage = powerImage(image, gamma)

  def fromRaw(image: BufferedImage): BufferedImage = powerImage(image, 1.0 / gamma)
}
