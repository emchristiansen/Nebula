package nebula

import breeze.linalg._
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
      val oversampleFactor = 8
      val numOversampledAngles = oversampleFactor * numAngles
      val numOversampledScales = oversampleFactor * numScales

      def pixelOffset(scaleIndex: Int, angleIndex: Int): DenseVector[Double] = {
        assert(scaleIndex < numOversampledScales)
        assert(angleIndex < numOversampledAngles)

        val angle = 2 * math.Pi * angleIndex.toDouble / numOversampledAngles
        val scale = {
          val base = math.exp(
            math.log(maxRadius / minRadius) / (numOversampledScales - 1))
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

  // Pad with |cols| - 1 columns of zeros on either side, and replicate
  // once vertically, dropping the last row.
  def prepareMatrixForConvolution(matrix: DenseMatrix[Double]): DenseMatrix[Double] = {
    val seqSeq = matrix.toSeqSeq

    val zeroPadding = IndexedSeq.fill(matrix.cols - 1)(0.0)
    val padded = seqSeq.map(zeroPadding ++ _ ++ zeroPadding)

    val replicated = (padded ++ padded.init).toMatrix
    assert(replicated.cols == 3 * matrix.cols - 2)
    assert(replicated.rows == 2 * matrix.rows - 1)
    replicated
  }

  def getResponseMap(
    normalizeByOverlap: Boolean,
    distance: (IndexedSeq[Double], IndexedSeq[Double]) => Double,
    base: DenseMatrix[Double],
    kernel: DenseMatrix[Double],
    angleIndices: Range,
    scaleIndices: Range): DenseMatrix[Double] = {
    require(2 * kernel.rows - 1 == base.rows)
    require(3 * kernel.cols - 2 == base.cols)
    require(angleIndices.min >= 0)
    require(angleIndices.max < kernel.rows)
    require(scaleIndices.min >= -kernel.cols + 1)
    require(scaleIndices.max < kernel.cols)

    val response = DenseMatrix.fill(angleIndices.size, scaleIndices.size)(0.0)
    for (angleIndex <- angleIndices; scaleIndex <- scaleIndices) {
      val scaleOffset = scaleIndex + kernel.cols - 1
      val baseVector = copy(base(
        angleIndex until angleIndex + kernel.rows,
        scaleOffset until scaleOffset + kernel.cols)).data.toIndexedSeq
      val kernelVector = kernel.data.toIndexedSeq
      assert(baseVector.size == kernelVector.size)
      response(angleIndex, scaleIndex) = distance(baseVector, kernelVector)
    }
    response
  }

  def getDistance(matcherType: MatcherType.MatcherType) = {
    matcherType match {
      case MatcherType.L1 => Matcher.l1[Double] _
      case MatcherType.L2 => Matcher.l2[Double] _
      case _ => sys.error("Not using supported distance")
    }
  }
}