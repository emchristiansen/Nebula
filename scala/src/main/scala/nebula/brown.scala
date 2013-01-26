package nebula

import org.opencv.features2d.DMatch
import java.awt.image.BufferedImage
import java.io.File
import nebula.util._
import org.apache.commons.io.FileUtils
import javax.imageio.ImageIO

///////////////////////////////////////////////////////////

case class BrownExperiment[E <% Extractor[F], M <% Matcher[F], F](
  datasetName: String,
  numMatches: Int,
  extractor: E,
  matcher: M)

object BrownExperiment {
  implicit class BrownExperiment2ExperimentRunnerWithRuntime[E <% Extractor[F], M <% Matcher[F], F](
    self: BrownExperiment[E, M, F])(
      implicit runtimeConfig: RuntimeConfig) extends ExperimentRunner[BrownExperimentResults[E, M, F]] {
    override def run = BrownExperimentResults(self)
  }

  implicit def brownExperiment2ExperimentRunner[E <% Extractor[F], M <% Matcher[F], F](
    self: BrownExperiment[E, M, F]): RuntimeConfig => ExperimentRunner[BrownExperimentResults[E, M, F]] = runtimeConfig => {
    implicit val iRC = runtimeConfig
    self
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
}

