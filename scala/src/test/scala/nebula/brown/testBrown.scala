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
  ignore("loadPatchPairs should return reasonable pairs", SlowTest, InteractiveTest, DatasetTest) {
    val datasetName = "liberty"
    val numMatches = 1000

    val patchPairs = PatchPair.loadPatchPairs(datasetName, numMatches, datasetRoot)

    for ((patchPair, index) <- patchPairs.zipWithIndex) {
      val sideBySide = GraphicsUtil.drawSideBySide(patchPair.left.image, patchPair.right.image)
      sideBySide.getGraphics.drawString(patchPair.corresponds.toString, 5, 20)

      TestUtil.dumpImage(s"${index}", sideBySide)
    }
  }

  test("ensure implicits are found", InstantTest) {
    val experiment = BrownExperiment(
      "liberty",
      1000,
      OpenCVExtractor.SIFT,
      Matcher.L2)

    implicit val runtimeConfig: RuntimeConfig = TestUtil.runtimeConfig

    val results = experiment.run
    val summary = results.to[ExperimentSummary]

    //    implicitly[nebula.BrownExperiment[nebula.OpenCVExtractor.SIFT.type,nebula.Matcher.L2.type,IndexedSeq[Double]] => 
    //      nebula.RuntimeConfig => 
    //        nebula.StorageInfo[
    //          nebula.BrownExperimentResults[nebula.OpenCVExtractor.SIFT.type,nebula.Matcher.L2.type,IndexedSeq[Double]]]]
    //    
    //    implicitly[nebula.BrownExperiment[nebula.OpenCVExtractor.SIFT.type,nebula.Matcher.L2.type,IndexedSeq[Double]] => 
    //      nebula.RuntimeConfig => 
    //        nebula.ExperimentRunner[
    //          nebula.BrownExperimentResults[nebula.OpenCVExtractor.SIFT.type,nebula.Matcher.L2.type,IndexedSeq[Double]]]]    

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