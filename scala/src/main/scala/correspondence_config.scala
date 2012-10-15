package nebula

import javax.imageio.ImageIO
import breeze.linalg._
import java.awt.image.BufferedImage

trait ImagePairLike {
  def leftImage: BufferedImage
  def rightImage: BufferedImage
}

case class WideBaselineExperiment(
  val imageClass: String,
  val otherImage: Int,
  val detector: Detector,
  val extractor: Extractor,
  val matcher: Matcher)

object WideBaselineExperiment {
  implicit def implicitExperiment(self: WideBaselineExperiment): Experiment =
    new Experiment {
      override def name = "WideBaselineExperiment"
      override def parameters = Seq(
        ("IC", self.imageClass),
        ("OI", self.otherImage.toString),
        ("D", Util.abbreviate(self.detector)),
        ("E", Util.abbreviate(self.extractor)),
        ("M", Util.abbreviate(self.matcher)))
      // TODO: asInstanceOf should not be necessary.
      override type ResultsType = WideBaselineExperimentResults
      override def run =
        WideBaselineExperimentResults.runExperiment(self)
    }

  implicit def implicitImagePairLike(self: WideBaselineExperiment): ImagePairLike = {
    new ImagePairLike {
      override def leftImage = {
        val file = Global.run[RuntimeConfig].projectChildPath("data/oxfordImages/%s/images/img1.bmp".format(self.imageClass))
        ImageIO.read(file)
      }
      override def rightImage = {
        val file = Global.run[RuntimeConfig].projectChildPath("data/oxfordImages/%s/images/img%s.bmp".format(self.imageClass, self.otherImage))
        ImageIO.read(file)
      }
    }
  }

  implicit def implicitHomography(self: WideBaselineExperiment) = new {
    def homography = {
      val homographyFile = Global.run[RuntimeConfig].projectChildPath("data/oxfordImages/%s/homographies/H1to%sp".format(self.imageClass, self.otherImage))
      Homography.fromFile(homographyFile)
    }
  }
}

case class SmallBaselineExperiment(
  val searchRadius: Int,
  val imageClass: String,
  val extractor: Extractor,
  val matcher: Matcher)

object SmallBaselineExperiment {
  implicit def implicitExperiment(self: SmallBaselineExperiment): Experiment =
    new Experiment {
      override def name = "SmallBaselineExperiment"
      override def parameters = Seq(
        ("SR", self.searchRadius.toString),
        ("IC", self.imageClass),
        ("E", Util.abbreviate(self.extractor)),
        ("M", Util.abbreviate(self.matcher)))
      override type ResultsType = Unit
      override def run = {
        val SmallBaselinePair(leftImage, rightImage, trueFlow) = SmallBaselinePair.fromName(
          Global.run[RuntimeConfig].projectChildPath("data/middleburyImages"),
          self.imageClass)

        val detector = DenseDetector()
        val keyPoints = detector.detect(leftImage)

        val (finalKeyPoints, leftDescriptors, rightDescriptors) = {
          val leftDescriptors = self.extractor.extract(leftImage, keyPoints)
          val rightDescriptors = self.extractor.extract(rightImage, keyPoints)

          val zipped = for (((keyPoint, Some(left)), Some(right)) <- keyPoints.zip(leftDescriptors).zip(rightDescriptors)) yield (keyPoint, left, right)
          (zipped.map(_._1), zipped.map(_._2), zipped.map(_._3))
        }

        println("Number of surviving KeyPoints: %s".format(leftDescriptors.size))

        def getDescriptorMatrix(descriptors: Seq[Descriptor]): DenseMatrix[Option[Descriptor]] = {
          val matrix = DenseMatrix.fill[Option[Descriptor]](leftImage.getHeight, leftImage.getWidth)(None)
          for ((keyPoint, descriptor) <- finalKeyPoints.zip(descriptors)) {
            matrix(keyPoint.pt.y.round.toInt, keyPoint.pt.x.round.toInt) = Some(descriptor)
          }
          matrix
        }

        val leftDescriptorsMatrix = getDescriptorMatrix(leftDescriptors)
        val rightDescriptorsMatrix = getDescriptorMatrix(rightDescriptors)

        // Match each left descriptor to its best right descriptor in the given radius.
        val flowTuples = for (
          leftRow <- 0 until leftDescriptorsMatrix.rows;
          leftColumn <- 0 until leftDescriptorsMatrix.cols;
          if leftDescriptorsMatrix(leftRow, leftColumn).isDefined
        ) yield {
          val leftDescriptor = leftDescriptorsMatrix(leftRow, leftColumn).get
          val rightPoints = for (
            rightRow <- math.max(0, leftRow - self.searchRadius) until math.min(rightDescriptorsMatrix.rows, leftRow + self.searchRadius);
            rightColumn <- math.max(0, leftColumn - self.searchRadius) until math.min(rightDescriptorsMatrix.cols, leftColumn + self.searchRadius);
            if rightDescriptorsMatrix(rightRow, rightColumn).isDefined
          ) yield {
            val rightDescriptor = rightDescriptorsMatrix(rightRow, rightColumn).get
            (rightRow, rightColumn, rightDescriptor)
          }

          val distances = self.matcher.doMatch(true, Seq(leftDescriptor), rightPoints.map(_._3)).map(_.distance)
          val bestIndex = distances.zipWithIndex.minBy(_._1)._2
          val (bestRightRow, bestRightColumn, _) = rightPoints(bestIndex)
          (leftRow, leftColumn, bestRightRow, bestRightColumn)
        }

        val flow = {
          val flow = DenseMatrix.fill[Option[FlowVector]](leftImage.getHeight, leftImage.getWidth)(None)
          for ((leftRow, leftColumn, bestRightRow, bestRightColumn) <- flowTuples) {
            val dX = bestRightColumn - leftColumn
            val dY = bestRightRow - leftRow
            flow(leftRow, leftColumn) = Some(FlowVector(dX, dY))
          }
          FlowField(flow)
        }

        println("l2 distance is: %.4f".format(flow.l2Distance(trueFlow)))
      }
    }
}
