package nebula.brown

import nebula._
import org.scalatest._
import javax.imageio.ImageIO
import java.io.File
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import nebula.util._
import nebula.summary._
import nebula.JsonProtocols._
import spray.json._
import nebula.brown._
import com.sun.xml.internal.bind.v2.model.runtime.RuntimeClassInfo

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
@WrapWith(classOf[ConfigMapWrapperSuite])
class TestBrown(val configMap: Map[String, Any]) extends ConfigMapFunSuite {
  test("loadPatchPairs should return reasonable pairs", SlowTest, InteractiveTest, DatasetTest) {
    val datasetName = "liberty"
    val numMatches = 1000

    val patchPairs = PatchPair.loadPatchPairs(datasetName, numMatches, datasetRoot)

    for ((patchPair, index) <- patchPairs.zipWithIndex) {
      val sideBySide = GraphicsUtil.drawSideBySide(patchPair.left.image, patchPair.right.image)
      sideBySide.getGraphics.drawString(patchPair.corresponds.toString, 5, 20)

      TestUtil.dumpImage(s"${index}", sideBySide)
    }
  }

  ignore("ensure implicits are found") {
    val experiment = BrownExperiment(
      "liberty",
      1000,
      OpenCVExtractor.SIFT,
      Matcher.L2)

    implicit val runtimeConfig: RuntimeConfig = TestUtil.runtimeConfig

    val results = experiment.run
    val summary = results.to[ExperimentSummary]  

    Distributed.unsafeCapstone(experiment)
  }

  test("brown toJson", InstantTest) {
    val experiment = BrownExperiment(
      "liberty",
      1000,
      OpenCVExtractor.SIFT,
      Matcher.L2)

    experiment.toJson
  }
}