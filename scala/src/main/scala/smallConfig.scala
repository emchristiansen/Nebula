package nebula

import breeze.linalg.DenseMatrix
import org.opencv.features2d.DMatch

case class SmallBaselineExperiment(
  val searchRadius: Int,
  val imageClass: String,
  val extractor: Extractor,
  val matcher: Matcher)

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
}

///////////////////////////////////////////////////////////

// TODO
case class SmallBaselineExperimentResults(
  val experiment: SmallBaselineExperiment,
  val dmatches: Seq[DMatch])

object SmallBaselineExperimentResults {
  def apply(experiment: SmallBaselineExperiment): SmallBaselineExperimentResults = {
    val noResults = SmallBaselineExperimentResults(experiment, sys.error(""))
    if (noResults.alreadyRun && Global.run[RuntimeConfig].skipCompletedExperiments) {
      val Some(file) = noResults.existingResultsFile
      println("Reading %s".format(file))
      IO.fromJSONFileAbstract[SmallBaselineExperimentResults](ExperimentIO.formats, file)
    } else run(experiment)
  }

  private def run(self: SmallBaselineExperiment): SmallBaselineExperimentResults = {
    val detector = DenseDetector()
    val keyPoints = detector.detect(self.leftImage)

    val (finalKeyPoints, leftDescriptors, rightDescriptors) = {
      val leftDescriptors = self.extractor.extract(self.leftImage, keyPoints)
      val rightDescriptors = self.extractor.extract(self.rightImage, keyPoints)

      val zipped = for (((keyPoint, Some(left)), Some(right)) <- keyPoints.zip(leftDescriptors).zip(rightDescriptors)) yield (keyPoint, left, right)
      (zipped.map(_._1), zipped.map(_._2), zipped.map(_._3))
    }

    println("Number of surviving KeyPoints: %s".format(leftDescriptors.size))

    def getDescriptorMatrix(descriptors: Seq[Descriptor]): DenseMatrix[Option[Descriptor]] = {
      val matrix = DenseMatrix.fill[Option[Descriptor]](self.leftImage.getHeight, self.leftImage.getWidth)(None)
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
      val flow = DenseMatrix.fill[Option[FlowVector]](self.leftImage.getHeight, self.leftImage.getWidth)(None)
      for ((leftRow, leftColumn, bestRightRow, bestRightColumn) <- flowTuples) {
        val dX = bestRightColumn - leftColumn
        val dY = bestRightRow - leftRow
        flow(leftRow, leftColumn) = Some(FlowVector(dX, dY))
      }
      FlowField(flow)
    }

    println("l2 distance is: %.4f".format(flow.l2Distance(self.groundTruth)))

    SmallBaselineExperimentResults(self, Seq())
  }

  implicit def implicitExperimentResults(self: SmallBaselineExperimentResults): ExperimentResults =
    new ExperimentResults {
      override def experiment = self.experiment
      override def save = sys.error("TODO")
    }
}