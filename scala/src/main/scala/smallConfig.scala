package nebula

import breeze.linalg.DenseMatrix
import org.opencv.features2d.DMatch

import java.awt.image.BufferedImage

import org.opencv.features2d.{ DMatch, KeyPoint }

case class SmallBaselineExperiment(
  searchRadius: Int,
  imageClass: String,
  extractor: Extractor,
  matcher: Matcher)

object SmallBaselineExperiment {
  implicit def implicitExperiment(self: SmallBaselineExperiment): Experiment with HasOriginal =
    new Experiment with HasOriginal {
      override def name = "SmallBaselineExperiment"
      override def parameters = Seq(
        ("SR", self.searchRadius.toString),
        ("IC", self.imageClass),
        ("E", Util.abbreviate(self.extractor)),
        ("M", Util.abbreviate(self.matcher)))
      override def original = self
    }

  implicit def implicitImagePairLike(self: SmallBaselineExperiment): ImagePairLike with HasGroundTruth[FlowField] =
    new ImagePairLike with HasGroundTruth[FlowField] {
      override val SmallBaselinePair(leftImage, rightImage, groundTruth) = SmallBaselinePair.fromName(
        Global.run[RuntimeConfig].projectChildPath("data/middleburyImages"),
        self.imageClass)
    }

  def estimateFlow(
    searchRadius: Int,
    extractor: Extractor,
    matcher: Matcher,
    leftImage: BufferedImage,
    rightImage: BufferedImage): FlowField = {
    val keyPoints = {
      val all = for (
        y <- 0 until leftImage.getHeight;
        x <- 0 until leftImage.getWidth
      ) yield new KeyPoint(
        x,
        y,
        -1,
        -1,
        -1,
        -1,
        -1)
      Global.random.shuffle(all).take(100)
    }

    //    println("Original number of KeyPoints: %s".format(keyPoints.size))

    val (finalKeyPoints, leftDescriptors, rightDescriptors) = {
      val leftDescriptors = extractor.extract(leftImage, keyPoints)
      val rightDescriptors = extractor.extract(rightImage, keyPoints)

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
        rightRow <- math.max(0, leftRow - searchRadius) until math.min(rightDescriptorsMatrix.rows, leftRow + searchRadius);
        rightColumn <- math.max(0, leftColumn - searchRadius) until math.min(rightDescriptorsMatrix.cols, leftColumn + searchRadius);
        if rightDescriptorsMatrix(rightRow, rightColumn).isDefined
      ) yield {
        val rightDescriptor = rightDescriptorsMatrix(rightRow, rightColumn).get
        (rightRow, rightColumn, rightDescriptor)
      }

      val distances = matcher.doMatch(true, Seq(leftDescriptor), rightPoints.map(_._3)).map(_.distance)
      val bestIndex = distances.zipWithIndex.minBy(_._1)._2
      val (bestRightRow, bestRightColumn, _) = rightPoints(bestIndex)
      (leftRow, leftColumn, bestRightRow, bestRightColumn)
    }

    val flow = DenseMatrix.fill[Option[FlowVector]](leftImage.getHeight, leftImage.getWidth)(None)
    for ((leftRow, leftColumn, bestRightRow, bestRightColumn) <- flowTuples) {
      val dX = bestRightColumn - leftColumn
      val dY = bestRightRow - leftRow
      flow(leftRow, leftColumn) = Some(FlowVector(dX, dY))
    }
    FlowField(flow)
  }

  implicit def implicitHasEstimate(self: SmallBaselineExperiment): HasEstimate[FlowField] =
    new HasEstimate[FlowField] {
      override def estimate = {
        estimateFlow(
          self.searchRadius,
          self.extractor,
          self.matcher,
          self.leftImage,
          self.rightImage)
      }
    }
}

///////////////////////////////////////////////////////////

// TODO
case class SmallBaselineExperimentResults(
  val experiment: SmallBaselineExperiment,
  val dmatches: Seq[DMatch])

object SmallBaselineExperimentResults {
  def apply(experiment: SmallBaselineExperiment): SmallBaselineExperimentResults = {
    val noResults = SmallBaselineExperimentResults(experiment, null)
    if (noResults.alreadyRun && Global.run[RuntimeConfig].skipCompletedExperiments) {
      val Some(file) = noResults.existingResultsFile
      println("Reading %s".format(file))
      IO.fromJSONFileAbstract[SmallBaselineExperimentResults](ExperimentIO.formats, file)
    } else run(experiment)
  }

  private def run(self: SmallBaselineExperiment): SmallBaselineExperimentResults = {
    println("l2 distance is: %.4f".format(self.estimate.l2Distance(self.groundTruth)))

    SmallBaselineExperimentResults(self, Seq())
  }

  implicit def implicitExperimentResults(self: SmallBaselineExperimentResults): ExperimentResults =
    new ExperimentResults {
      override def experiment = self.experiment
      override def save = sys.error("TODO")
    }
}