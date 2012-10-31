import org.scalatest.FunSuite
import org.opencv.features2d._
import javax.imageio.ImageIO
import java.io.File
import org.opencv.core.MatOfKeyPoint
import org.opencv.features2d.{ FeatureDetector, KeyPoint }
import nebula._
import org.apache.xmlgraphics.image.loader.ImageManager
import org.opencv.core.Mat
import org.opencv.core.MatOfKeyPoint
import wideBaseline._
import com.twitter.util.Eval

import OpenCVDetectorType._

class TestConfig extends FunSuite {
  test("toString on WideBaselineExperiment should stay the same when it's upcast") {
    val wide = WideBaselineExperiment(
      "wall",
      2,
      OpenCVDetector(FAST, Some(100)),
      PatchExtractor(
        SortExtractor(),
        false,
        false,
        8,
        5,
        "Gray"),
      L1Matcher())
    val wideString = wide.toString

    val experiment: Experiment = wide
    val experimentString = experiment.toString
    assert(wideString === experimentString)

    assert(experiment === eval[Experiment](experiment.toString))
    assert(experiment.toString === eval[Experiment](experiment.toString).toString)
  }
}