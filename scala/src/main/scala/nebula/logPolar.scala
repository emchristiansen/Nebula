package nebula

import java.awt.image.BufferedImage

import org.opencv.features2d.KeyPoint

import breeze.linalg.{DenseMatrix, DenseVector, copy}
import nebula.util.DenseMatrixUtil._
import nebula.util.imageProcessing.ImageUtil
import nebula.util.imageProcessing.RichImage.bufferedImage

///////////////////////////////////////////////////////////

object LogPolar {
  val epsilon = 0.00001

  // Get the minimum, maximum, and exponential base of the scaling
  // factors. These are used to create the image pyramid from
  // which descriptors are extracted.
  // TODO: Why doesn't this just return the factors directly?
  def getFactors(
    samplingRadius: Double,
    minRadius: Double,
    maxRadius: Double,
    numScales: Int): Tuple3[Double, Double, Double] = {
    val maxScalingFactor = samplingRadius / minRadius
    val minScalingFactor = samplingRadius / maxRadius
    assert(maxScalingFactor > minScalingFactor)
    val base = math.exp(math.log(minScalingFactor / maxScalingFactor) / (numScales - 1))
    (minScalingFactor, maxScalingFactor, base)
  }

  // Get the image pyramid, as well as the scaling factors that
  // were used to create it. Since the ideal scaling factors cannot be
  // used in all cases (integer rounding), we return the ideal factors
  // plus the actual factors used.
  def scaleImage(
    samplingRadius: Double,
    minRadius: Double,
    maxRadius: Double,
    numScales: Int,
    blurWidth: Int,
    image: BufferedImage): Tuple3[IndexedSeq[Double], IndexedSeq[Tuple2[Double, Double]], IndexedSeq[BufferedImage]] = {
    // We build a set of rescaled images. The biggest image is scaled
    // so that |minRadius| in the original image is |samplingRadius|
    // in the scaled image. The smallest image is scaled so that
    // |maxRadius| maps to |samplingRadius|.

    val (minScalingFactor, maxScalingFactor, base) =
      getFactors(samplingRadius, minRadius, maxRadius, numScales)
    assert(base < 1)

    val idealScaleFactors = for (scaleIndex <- 0 until numScales) yield {
      val scaleFactor = maxScalingFactor * math.pow(base, scaleIndex)
      assert(scaleFactor >= minScalingFactor - epsilon)
      assert(scaleFactor <= maxScalingFactor + epsilon)
      if (scaleIndex == 0) assertNear2(scaleFactor, maxScalingFactor)
      if (scaleIndex == numScales - 1) assertNear2(scaleFactor, minScalingFactor)
      scaleFactor
    }

    val blurred = ImageUtil.boxBlur(blurWidth, image)

    val (realFactors, scaledImages) = (for (scaleFactor <- idealScaleFactors) yield {
      ImageUtil.scale(scaleFactor, blurred)
    }) unzip

    (idealScaleFactors, realFactors, scaledImages)
  }

  def rawLogPolarSeq(
    normalizeScale: Boolean,
    minRadius: Double,
    maxRadius: Double,
    numScales: Int,
    numAngles: Int,
    blurWidth: Int)(
      image: BufferedImage,
      keyPoints: Seq[KeyPoint]): Seq[Option[DenseMatrix[Int]]] = {
    // The larger this number, the more accurate the sampling
    // but the larger the largest resized image.
    val samplingRadius = 4.0

    val (idealScaleFactors, realScaleFactors, scaledImages) = scaleImage(
      samplingRadius,
      minRadius,
      maxRadius,
      numScales,
      blurWidth,
      image)

    for (keyPoint <- keyPoints) yield {
      def isInsideBounds(keyPoint: KeyPoint): Boolean = {
        val x = keyPoint.pt.x * realScaleFactors.last._1
        val y = keyPoint.pt.y * realScaleFactors.last._2
        val width = scaledImages.last.getWidth
        val height = scaledImages.last.getHeight

        x - epsilon > samplingRadius &&
          x + epsilon + samplingRadius < width &&
          y - epsilon > samplingRadius &&
          y + epsilon + samplingRadius < height
      }

      if (!isInsideBounds(keyPoint)) None
      else {
        val matrix = DenseMatrix.fill(numAngles, numScales)(0)
        for (scaleIndex <- 0 until numScales; angleIndex <- 0 until numAngles) {
          val scaledImage = scaledImages(scaleIndex)
          val (scaleFactorX, scaleFactorY) = realScaleFactors(scaleIndex)

          val (scaledX, scaledY) = (
            scaleFactorX * keyPoint.pt.x,
            scaleFactorY * keyPoint.pt.y)

          val angle = 2 * math.Pi * angleIndex.toDouble / numAngles
          val pixelOffset = DenseVector(
            samplingRadius * math.sin(angle),
            samplingRadius * math.cos(angle))

          val (x, y) = (scaledX + pixelOffset(0), scaledY + pixelOffset(1))
          val pixel = scaledImage.getSubPixel(x, y).get

          matrix(angleIndex, scaleIndex) = pixel.gray.head
        }
        Some(matrix)
      }
    }
  }

  def rawLogPolar(
    normalizeScale: Boolean,
    minRadius: Double,
    maxRadius: Double,
    numScales: Int,
    numAngles: Int,
    blurWidth: Int)(
      image: BufferedImage,
      keyPoint: KeyPoint): Option[DenseMatrix[Int]] = {
    rawLogPolarSeq(
      normalizeScale,
      minRadius,
      maxRadius,
      numScales,
      numAngles,
      blurWidth)(
        image,
        Seq(keyPoint)).head
  }

//  // Pad with |cols| - 1 columns of zeros on either side, and replicate
//  // once vertically, dropping the last row.
//  def prepareMatrixForConvolution(matrix: DenseMatrix[Double]): DenseMatrix[Double] = {
//    val seqSeq = matrix.toSeqSeq
//
//    val zeroPadding = IndexedSeq.fill(matrix.cols - 1)(0.0)
//    val padded = seqSeq.map(zeroPadding ++ _ ++ zeroPadding)
//
//    val replicated = (padded ++ padded.init).toMatrix
//    assert(replicated.cols == 3 * matrix.cols - 2)
//    assert(replicated.rows == 2 * matrix.rows - 1)
//    replicated
//  }

//  def stackVertical(matrix: DenseMatrix[Double]): DenseMatrix[Double] = {
//    val seqSeq = matrix.toSeqSeq
//    (seqSeq ++ seqSeq.init).toMatrix
//  }

  def getResponseMap[A, B](
    normalizer: Normalizer[DenseMatrix[A], DenseMatrix[B]],
    normalizeByOverlap: Boolean,
    distance: Matcher.DescriptorDistance[B],
    base: DenseMatrix[A],
    kernel: DenseMatrix[B],
    angleIndices: Range,
    scaleIndices: Range): DenseMatrix[Double] = {
    require(2 * kernel.rows - 1 == base.rows)
    require(kernel.cols == base.cols)
    require(angleIndices.min >= 0)
    require(angleIndices.max < kernel.rows)
    require(scaleIndices.min >= -kernel.cols + 1)
    require(scaleIndices.max < kernel.cols)

    // If |normalization| isn't raw, we do match-time normalization
    // of the part of the log-polar patterns that overlap.
    def normalize(patch: DenseMatrix[Double]): IndexedSeq[Double] = {
      if (normalization == PatchExtractorType.Raw) 
        patch.toSeqSeq.flatten
      else {
        val extractor = PatchExtractor.constructorDouble(normalization)
        extractor(patch.toSeqSeq.flatten).values[Double]
      }
    }
    
    val response = DenseMatrix.fill(angleIndices.size, scaleIndices.size)(0.0)
    for (angleIndex <- angleIndices; scaleIndex <- scaleIndices) {
      val baseScaleRange =
        math.max(0, scaleIndex) until math.min(base.cols, base.cols + scaleIndex)
      val kernelScaleRange =
        math.max(0, -scaleIndex) until math.min(base.cols, base.cols - scaleIndex)

      val baseMatrix = copy(base(
        angleIndex until angleIndex + kernel.rows,
        baseScaleRange))
      val kernelMatrix = copy(kernel(
        ::,
        kernelScaleRange))
        
      val baseVector = normalize(baseMatrix)
      val kernelVector = normalize(kernelMatrix)
      assert(baseVector.size == kernelVector.size)
      
      val scaleOffset = scaleIndex - scaleIndices.min
      val unnormalized = distance(baseVector, kernelVector)
      response(angleIndex, scaleOffset) = if (normalizeByOverlap) {
        val denominator = kernel.cols - scaleIndex.abs
        unnormalized / denominator
      } else unnormalized
    }
    response
  }

  //  def getResponseMap(
  //    normalizeByOverlap: Boolean,
  //    distance: (IndexedSeq[Double], IndexedSeq[Double]) => Double,
  //    base: DenseMatrix[Double],
  //    kernel: DenseMatrix[Double],
  //    angleIndices: Range,
  //    scaleIndices: Range): DenseMatrix[Double] = {
  //    require(2 * kernel.rows - 1 == base.rows)
  //    require(3 * kernel.cols - 2 == base.cols)
  //    require(angleIndices.min >= 0)
  //    require(angleIndices.max < kernel.rows)
  //    require(scaleIndices.min >= -kernel.cols + 1)
  //    require(scaleIndices.max < kernel.cols)
  //
  //    val response = DenseMatrix.fill(angleIndices.size, scaleIndices.size)(0.0)
  //    for (angleIndex <- angleIndices; scaleIndex <- scaleIndices) {
  //      val scaleOffset = scaleIndex + kernel.cols - 1
  ////      println(scaleIndices, scaleOffset)
  //      val baseVector = copy(base(
  //        angleIndex until angleIndex + kernel.rows,
  //        scaleOffset until scaleOffset + kernel.cols)).data.toIndexedSeq
  //      val kernelVector = kernel.data.toIndexedSeq
  //      assert(baseVector.size == kernelVector.size)
  //      response(angleIndex, scaleOffset) = distance(baseVector, kernelVector)
  //    }
  //    response
  //  }

//  def getDistance(matcherType: MatcherType.MatcherType) = {
//    matcherType match {
//      case MatcherType.L1 => Matcher.l1[Double] _
//      case MatcherType.L2 => Matcher.l2[Double] _
//      case _ => sys.error("Not using supported distance")
//    }
//  }
}
//
//    try {
//      // TODO: Make parameter
//      val oversampleFactor = 8
//      val numOversampledAngles = oversampleFactor * numAngles
//      val numOversampledScales = oversampleFactor * numScales
//
//      def pixelOffset(scaleIndex: Int, angleIndex: Int): DenseVector[Double] = {
//        assert(scaleIndex < numOversampledScales)
//        assert(angleIndex < numOversampledAngles)
//
//        val angle = 2 * math.Pi * angleIndex.toDouble / numOversampledAngles
//        val scale = {
//          val base = math.exp(
//            math.log(maxRadius / minRadius) / (numOversampledScales - 1))
//          val scale = minRadius * math.pow(base, scaleIndex)
//          assert(scale >= minRadius, "scale, minRadius: %s, %s".format(scale, minRadius))
//          assert(scale <= maxRadius + .0001, "scale, maxRadius: %s, %s".format(scale, maxRadius))
//          if (scaleIndex == 0) assert(scale == minRadius)
//          if (scaleIndex == numOversampledScales - 1) assertNear(0.0001, scale, maxRadius)
//          scale
//        }
//        DenseVector(scale * math.sin(angle), scale * math.cos(angle))
//      }
//
//      val oversampled = {
//        val image = new BufferedImage(
//          numOversampledScales,
//          numOversampledAngles,
//          BufferedImage.TYPE_BYTE_GRAY)
//        for (
//          angleIndex <- 0 until numOversampledAngles;
//          scaleIndex <- 0 until numOversampledScales
//        ) {
//          image.setPixel(
//            scaleIndex,
//            angleIndex,
//            pixel(pixelOffset(scaleIndex, angleIndex)))
//        }
//        image
//      }
//
//      val scaledImage = ImageUtil.scale(1.0 / oversampleFactor, oversampled)
//      assert(scaledImage.getWidth == numScales)
//      assert(scaledImage.getHeight == numAngles)
//      Some(scaledImage.toMatrix)
//    } catch {
//      case _: NoSuchElementException => None
//    }
//  }