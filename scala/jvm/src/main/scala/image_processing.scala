package nebula

import java.awt.Rectangle
import java.awt.geom._
import java.awt.image._
import java.awt.image.AffineTransformOp._
import java.io.File
import javax.imageio.ImageIO

class LazyImage(originalPath: String, condition: MPIECondition, roiString: String) {
  private val mpieProperties = MPIEProperties.parseMPIEPath(originalPath)

  val id = mpieProperties.id
  // A larger value reduces the probability an ROI will try to access an
  // out-of-bounds pixel, but increases computation time.
  val padding = 200

  lazy val path = { 
    val image = ImageIO.read(new File(originalPath))
    
    def warp(image: BufferedImage): BufferedImage = {
      // first get the file with the alignment matrix in it
      val pathSegment = mpieProperties.pathSegment
      val filename = "%s_%s_%s_%s_mean_align.txt".format(mpieProperties.id, mpieProperties.session, mpieProperties.expression, mpieProperties.pose)
      val path = "%s/processed/%s/%s".format(Global.run.piSliceRoot, pathSegment, filename)

      val tformParams = io.Source.fromFile(path).mkString.split("\t|\n").map(_.toDouble)

      val transformMatrix = new AffineTransform(
	tformParams(0), tformParams(3), // column 1
	tformParams(1), tformParams(4), // column 2
	tformParams(2), tformParams(5)) // column 3

      val transformOp = new AffineTransformOp(transformMatrix, TYPE_BILINEAR);

      val warped = transformOp.filter(image, null)
      Image.transparentToGreen(warped)
    }

    def scale(image: BufferedImage): BufferedImage = { 
      val scaleMatrix = new AffineTransform(Global.run.scaleFactor, 0, 0, Global.run.scaleFactor, 0, 0)
      val scaleOp = new AffineTransformOp(scaleMatrix, TYPE_BILINEAR)
      scaleOp.filter(image, null)
    }

    def illumination(image: BufferedImage): BufferedImage = {
      val illum = condition.illumination
      if (!illum.contains("x")) {
	image
      } else {
	// When doing artificial illumination, we start with neutral.
	assert(mpieProperties.illumination == "00")

	val factor = illum.init.toDouble

	val raw = Image.toRaw(image)
	for (y <- 0 until raw.getHeight;
	     x <- 0 until raw.getWidth) {
	  val pixel = Pixel.scale(Pixel.getPixel(raw, x, y), factor)
	  raw.setRGB(x, y, pixel.argb)
	}
	Image.fromRaw(raw)
      }
    }

    def blur(image: BufferedImage): BufferedImage = {
      val std = condition.blur.toDouble
      if (std == 0) {
	image
      } else {
	val kernelData = Util.gaussianKernel(std)
	val kernel = new Kernel(kernelData.size, kernelData.size, kernelData.flatten.toArray.map(_.toFloat))
	val op = new ConvolveOp(kernel, ConvolveOp.EDGE_ZERO_FILL, null)
	op.filter(image, null)
      }
    }

    def noise(image: BufferedImage): BufferedImage = {
      val std = condition.noise.toDouble
      if (std != 0) {
	for (y <- 0 until image.getHeight;
	     x <- 0 until image.getWidth) {
	  val pixel = Pixel.getPixel(image, x, y)
	  val List(rNoise, gNoise, bNoise) = (0 until 3).toList.map(_ => Global.random.nextGaussian * std).map(_.toInt)
	  val sum = Pixel.add(pixel, rNoise, gNoise, bNoise)
	  image.setRGB(x, y, sum.argb)
	}
      }
      image
    }

    def jpeg(image: BufferedImage): BufferedImage = {
      val quality = condition.jpeg.toFloat
      if (quality == 0) {
	image
      } else {
	throw new Exception("not implemented")
      }
    }

    def misalignment(image: BufferedImage): BufferedImage = {
      val std = condition.misalignment.toDouble

      if (std == 0) {
	image
      } else {
	val fiducials = {
	  val Parser = """.*\((\S+), (\S+)\)""".r

	  val pathSegment = mpieProperties.pathSegment
	  val filename = "%s_%s_%s_%s_mean_fiducials.txt".format(mpieProperties.id, mpieProperties.session, mpieProperties.expression, mpieProperties.pose)
	  val path = "%s/processed/%s/%s".format(Global.run.piSliceRoot, pathSegment, filename)
	  val lines = io.Source.fromFile(path).mkString.split("\n").filter(_.size > 0)

	  val all = for (l <- lines) yield {
	    val Parser(x, y) = l
	    (padding + x.toDouble * Global.run.scaleFactor, padding + y.toDouble * Global.run.scaleFactor)
	  }

	  // We 4 fiducials for the misalignment for each pose. Note the frontal pose
	  // has 6 fiducials, so we drop the fiducials at the inside corners of the eyes. These
	  // are currently at locations 1 and 2.
	  // TODO: Stop assuming the fiducials come in a particular order.
	  val culled = if (all.size == 6) {
	    all.take(1) ++ all.drop(3)
	  } else {
	    all
	  }
	  assert(culled.size == 4)
	  culled.toList
	}

	def perturb(xy: Tuple2[Double, Double]): Tuple2[Double, Double] = {
	  val xNoise = Global.random.nextGaussian * std
	  val yNoise = Global.random.nextGaussian * std

	  val (x, y) = xy
	  (x + xNoise, y + yNoise)
	}

	val perturbed = fiducials.map(perturb)

	val tformParams = Util.fitSimilarity(fiducials, perturbed).flatten

	val transformMatrix = new AffineTransform(
	  tformParams(0), tformParams(3), // column 1
	  tformParams(1), tformParams(4), // column 2
	  tformParams(2), tformParams(5)) // column 3

	val transformOp = new AffineTransformOp(transformMatrix, TYPE_BILINEAR);

	val transformed = new BufferedImage(image.getWidth, image.getHeight, image.getType)
	transformOp.filter(image, transformed)

	assert(transformed.getWidth == image.getWidth && transformed.getHeight == image.getHeight)

	Image.transparentToGreen(transformed)
      }
    }

    def padImage(image: BufferedImage): BufferedImage = {
      val padded = new BufferedImage(2 * padding + image.getWidth, 2 * padding + image.getHeight, image.getType)
      for (y <- 0 until image.getHeight;
	   x <- 0 until image.getWidth) {
	     padded.setRGB(padding + x, padding + y, image.getRGB(x, y))
      }
      padded
    }

    def background(unpaddedImage: BufferedImage): BufferedImage = {
      val image = padImage(unpaddedImage)

      val mask = { 
	val pathSegment = mpieProperties.pathSegment
	val filename = "%s_%s_%s_%s_mean_alpha.png".format(mpieProperties.id, mpieProperties.session, mpieProperties.expression, mpieProperties.pose)
	val path = "%s/processed/%s/%s".format(Global.run.piSliceRoot, pathSegment, filename)
	padImage(scale(warp(ImageIO.read(new File(path)))))
      }

      lazy val syntheticBackground = { 
	val directory = "%s/session%s".format(Global.run.backgroundRoot, mpieProperties.session)
	val file = { 
	  val files = (new File(directory)).listFiles.toList.filter(!_.toString.contains(".DS_Store"))
	  Global.random.shuffle(files).head
	}
	val synthetic = ImageIO.read(file)
	val x = Global.random.nextInt(synthetic.getWidth - image.getWidth)
	val y = Global.random.nextInt(synthetic.getHeight - image.getHeight)
	synthetic.getSubimage(x, y, image.getWidth, image.getHeight)
      }

      for (h <- 0 until image.getHeight;
	   w <- 0 until image.getWidth) {
	val gray = { 
	  val Pixel(_, r, g, b) = Pixel.getPixel(mask, w, h)
	  (r + g + b) / 3
	}
	if (gray < 127) {
	  condition.background match {
	    case "synthetic" => image.setRGB(w, h, syntheticBackground.getRGB(w, h))
	    case "blank" => image.setRGB(w, h, Pixel.green)
	    case _ => throw new Exception
	  }
	}
      }
     
      image
    }

    def roi(image: BufferedImage): BufferedImage = { 
      // figure out the path to the mask
      val poseUnderscore = condition.pose match { 
	case "240" => "24_0"
	case "190" => "19_0"
	case "051" => "05_1"
      }
      
      val roiPath = Global.run.piSliceRoot ++ "/processed/roi/" ++ poseUnderscore ++ "/" ++ roiString ++ ".png"
      val roiImg = padImage(scale(ImageIO.read(new File(roiPath))))
      
      Image.extractROI(roiImg, image)
    }

    val scaled = scale(warp(image))
    // TODO: The order of operations matters, and it might be more correct if background is split
    // into two phases, so that blur / noise / etc applies to sythetic backgrounds but not
    // greenscreens.
    val conditioned = roi(misalignment(background(jpeg(noise(blur(illumination(scaled)))))))

    val tempPrefix = "%s_%s_%s".format(id, roiString, condition.toString)
    val path = Util.createTempFile(tempPrefix, ".png")

    if (Global.run.deleteTemporaryFiles) path.deleteOnExit

    ImageIO.write(conditioned, "png", path)
    path.toString
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
