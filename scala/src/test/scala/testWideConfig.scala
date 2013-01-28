import nebula._
import org.scalatest.FunSuite
import javax.imageio.ImageIO
import java.io.File
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import nebula.util._
import nebula.summary._
import nebula.wideBaseline.WideBaselineExperiment
import nebula.JsonProtocols._

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
class TestWideConfig extends FunSuite {
  ignore("ensure implicits are found") {
    val experiment = WideBaselineExperiment(
      "wall",
      2,
      OpenCVDetector.FAST,
      OpenCVExtractor.SIFT,
      Matcher.L2)
      
    implicit val runtimeConfig = TestUtil.runtimeConfig
    
    val results = experiment.run
    val summary = results.to[ExperimentSummary]
    
    Distributed.unsafeCapstone(experiment)
  }
}