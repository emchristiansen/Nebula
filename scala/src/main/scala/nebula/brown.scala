package nebula

import org.opencv.features2d.DMatch
import java.awt.image.BufferedImage
import java.io.File
import nebula.util._
import org.apache.commons.io.FileUtils
import javax.imageio.ImageIO
import nebula.summary._

///////////////////////////////////////////////////////////

case class BrownExperiment[E <% Extractor[F], M <% Matcher[F], F](
  datasetName: String,
  numMatches: Int,
  extractor: E,
  matcher: M)

object BrownExperiment {
  implicit class BrownExperiment2ExperimentRunnerWithRuntime[E <% Extractor[F], M <% Matcher[F], F](
    self: BrownExperiment[E, M, F])(
      runtimeConfig: RuntimeConfig) extends ExperimentRunner[BrownExperimentResults[E, M, F]] {
    private implicit val iRC = runtimeConfig

    override def run = BrownExperimentResults(self)
  }
}

///////////////////////////////////////////////////////////

case class BrownExperimentResults[E, M, F](
  experiment: BrownExperiment[E, M, F],
  dmatches: Seq[DMatch])

object BrownExperimentResults {
  def apply[E <% Extractor[F], M <% Matcher[F], F](
    experiment: BrownExperiment[E, M, F])(
      implicit runtimeConfig: RuntimeConfig): BrownExperimentResults[E, M, F] = {
    val patchPairs = PatchPair.loadPatchPairs(
      experiment.datasetName,
      experiment.numMatches,
      runtimeConfig.dataRoot)

    val dmatchOptions = patchPairs map {
      patchPair =>
        {
          val distance = patchPair.getDistance(experiment.extractor, experiment.matcher)
          for (d <- distance) yield new DMatch(patchPair.left.id, patchPair.right.id, d.toFloat)
        }
    }

    BrownExperimentResults(experiment, dmatchOptions.flatten)
  }

  implicit def brownExperimentResults2ExperimentSummary[E, M, F](
    self: BrownExperimentResults[E, M, F])(runtimeConfig: RuntimeConfig) = {
    implicit val iRC = runtimeConfig
    ExperimentSummary(
      Map(
        "recognitionRate" -> (() => SummaryUtil.recognitionRate(self.dmatches))),
      Map())
  }
}
