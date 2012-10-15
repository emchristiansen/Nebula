package nebula

import java.io.File
import xml._

import breeze.linalg._

import org.opencv.core.Mat
import org.opencv.features2d.{ DMatch, KeyPoint }
import org.opencv.highgui.Highgui.imread

import DetectorImpl._
import ExtractorImpl._
import MatcherImpl._

case class PredictionAndTruth(val prediction: Double, val truth: Boolean)

case class ResultsData(val predictionsAndTruths: Seq[PredictionAndTruth])

object ResultsData {
  def sorted(predictions: Seq[Double], truths: Seq[Boolean]): ResultsData = {
    val sortedPairs = predictions.zip(truths).sortBy(_._1)
    val predictionsAndTruths =
      sortedPairs.map(e => PredictionAndTruth(e._1, e._2))
    ResultsData(predictionsAndTruths)
  }
}

trait ExperimentResults {
  def experiment: Experiment
  def save: Unit

  ///////////////////////////////////////////////////////////

  def filenameNoTime: String =
    experiment.name + "_" + experiment.parameters.map(p => p._1 + "-" + p._2).mkString("_") + ".json"

  def filename: String = experiment.unixEpoch + "_" + filenameNoTime

  def outDirectory: File = Global.run[RuntimeConfig].projectChildPath("results/experiment_data")

  def path: File = new File(outDirectory, filename)

  def existingResultsFiles: Seq[File] = {
    val allPaths = outDirectory.list.toList.map(path => outDirectory + "/" + path.toString)
    val matchingPaths = allPaths.filter(_.contains(filenameNoTime))
    matchingPaths.sortBy(identity).reverse.map(path => new File(path))
  }

  def existingResultsFile: Option[File] = {
    existingResultsFiles match {
      case Nil => None
      case file :: _ => Some(file)
    }
  }

  def alreadyRun: Boolean = !existingResultsFile.isEmpty
}

///////////////////////////////////////////////////////////

// TODO
case class SmallBaselineExperimentResults(
  val experiment: SmallBaselineExperiment,
  val dmatches: Seq[DMatch])

object SmallBaselineExperimentResults {
  def apply(experiment: SmallBaselineExperiment): SmallBaselineExperimentResults = {
    val noResults = SmallBaselineExperimentResults(experiment, sys.error(""))
    if (noResults.alreadyRun) {
      val Some(file) = noResults.existingResultsFile
      println("Reading %s".format(file))
      IO.fromJSONFileAbstract[SmallBaselineExperimentResults](ExperimentIO.formats, file)
    } else run(experiment)
  }

  private def run(self: SmallBaselineExperiment): SmallBaselineExperimentResults = {
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

    SmallBaselineExperimentResults(self, Seq())
  }

  implicit def implicitExperimentResults(self: SmallBaselineExperimentResults): ExperimentResults =
    new ExperimentResults {
      override def experiment = self.experiment
      override def save = sys.error("TODO")
    }
}

///////////////////////////////////////////////////////////

case class WideBaselineExperimentResults(
  val experiment: WideBaselineExperiment,
  val dmatches: Seq[DMatch])

object WideBaselineExperimentResults {
  def apply(
    experiment: WideBaselineExperiment): WideBaselineExperimentResults = {
    val noResults = WideBaselineExperimentResults(experiment, sys.error(""))
    if (noResults.alreadyRun) {
      val Some(file) = noResults.existingResultsFile
      println("Reading %s".format(file))
      IO.fromJSONFileAbstract[WideBaselineExperimentResults](ExperimentIO.formats, file)
    } else run(experiment)
  }

  private def run(self: WideBaselineExperiment): WideBaselineExperimentResults = {
    println("Running %s".format(self))

    val leftImage = self.leftImage
    val rightImage = self.rightImage

    val (leftKeyPoints, rightKeyPoints) = {
      val leftKeyPoints = self.detector.detect(leftImage)
      Util.pruneKeyPoints(
        leftImage,
        rightImage,
        self.homography,
        leftKeyPoints).unzip
    }

    println("Number of KeyPoints: %s".format(leftKeyPoints.size))

    val (leftDescriptors, rightDescriptors) = {
      val leftDescriptors = self.extractor.extract(leftImage, leftKeyPoints)
      val rightDescriptors = self.extractor.extract(rightImage, rightKeyPoints)

      for ((Some(left), Some(right)) <- leftDescriptors.zip(rightDescriptors)) yield (left, right)
    } unzip

    println("Number of surviving KeyPoints: %s".format(leftDescriptors.size))

    val dmatches = self.matcher.doMatch(true, leftDescriptors, rightDescriptors)

    val results = WideBaselineExperimentResults(self, dmatches)
    results.save
    results
  }

  implicit def implicitExperimentResults(self: WideBaselineExperimentResults): ExperimentResults =
    new ExperimentResults {
      override def experiment = self.experiment
      override def save = {
        println("Writing to %s".format(self.path))
        IO.toJSONFileAbstract(
          ExperimentIO.formats,
          WideBaselineExperimentResults.this,
          self.path)
      }
    }
}

