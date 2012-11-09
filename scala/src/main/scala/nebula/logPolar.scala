package nebula

import breeze.linalg.DenseMatrix
import nebula.util.imageProcessing.Pixel
import org.opencv.features2d.KeyPoint
import java.awt.image.BufferedImage
import breeze.linalg.DenseVector
import nebula.util.imageProcessing.ImageUtil
import nebula.util.imageProcessing.RichImage._
import nebula.util.DenseMatrixUtil._

///////////////////////////////////////////////////////////

object LogPolar {
  def rawLogPolar(
    normalizeScale: Boolean,
    minRadius: Double,
    maxRadius: Double,
    numScales: Int,
    numAngles: Int,
    blurWidth: Int)(
      image: BufferedImage,
      keyPoint: KeyPoint): Option[DenseMatrix[Int]] = {
    assert(!normalizeScale)

    val blurred = ImageUtil.boxBlur(blurWidth, image)

    def pixel(offset: DenseVector[Double]): Pixel = {
      val (x, y) = (keyPoint.pt.x + offset(0), keyPoint.pt.y + offset(1))
      blurred.getSubPixel(x, y).get
    }

    try {
      // TODO: Make parameter
      val oversampleFactor = 4
      val numOversampledAngles = oversampleFactor * numAngles
      val numOversampledScales = oversampleFactor * numScales

      def pixelOffset(scaleIndex: Int, angleIndex: Int): DenseVector[Double] = {
        assert(scaleIndex < numOversampledScales)
        assert(angleIndex < numOversampledAngles)

        val angle = 2 * math.Pi * angleIndex.toDouble / numOversampledAngles
        val scale = {
          val base = math.exp(
              math.log(maxRadius / minRadius) / (numOversampledScales - 1))
          // Minus epsilon to deal with precision errors.
          val scale = minRadius * math.pow(base, scaleIndex)
          assert(scale >= minRadius, "scale, minRadius: %s, %s".format(scale, minRadius))
          assert(scale <= maxRadius + .0001, "scale, maxRadius: %s, %s".format(scale, maxRadius))
          if (scaleIndex == 0) assert(scale == minRadius)
          if (scaleIndex == numOversampledScales - 1) assertNear(0.0001, scale, maxRadius)
          scale
        }
        DenseVector(scale * math.sin(angle), scale * math.cos(angle))
      }

      val oversampled = {
        val image = new BufferedImage(
          numOversampledScales,
          numOversampledAngles,
          BufferedImage.TYPE_BYTE_GRAY)
        for (
          angleIndex <- 0 until numOversampledAngles;
          scaleIndex <- 0 until numOversampledScales
        ) {
          image.setPixel(
            scaleIndex,
            angleIndex,
            pixel(pixelOffset(scaleIndex, angleIndex)))
        }
        image
      }

      val scaledImage = ImageUtil.scale(1.0 / oversampleFactor, oversampled)
      assert(scaledImage.getWidth == numScales)
      assert(scaledImage.getHeight == numAngles)
      Some(scaledImage.toMatrix)
    } catch {
      case _: NoSuchElementException => None
    }
  }
}