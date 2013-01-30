package nebula.util

import org.scalatest.FunSuite
import org.opencv.features2d._
import javax.imageio.ImageIO
import java.io.File
import org.opencv.core.MatOfKeyPoint
import org.opencv.features2d.{ FeatureDetector, KeyPoint }
import nebula._
//import org.apache.xmlgraphics.image.loader.ImageManager
import org.opencv.core.Mat
import org.opencv.core.MatOfKeyPoint
import nebula.util.JSONUtil
import spray.json.DefaultJsonProtocol
import spray.json.JsArray
import spray.json.RootJsonFormat
import spray.json.JsValue
import spray.json.JsNumber
import spray.json.JsString

import wideBaseline._
import smallBaseline._

import spray.json._

import nebula.util.Memoize._
import nebula.util._

import breeze.linalg._

import JsonProtocols._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import TestUtil._

import org.scalatest.FunSuite
import org.opencv.features2d._
import javax.imageio.ImageIO
import java.io.File
import org.opencv.core.MatOfKeyPoint
import org.opencv.features2d.{ FeatureDetector, KeyPoint }
import nebula._
//import org.apache.xmlgraphics.image.loader.ImageManager
import org.opencv.core.Mat
import java.awt.Color
import java.awt.image.BufferedImage
import org.apache.commons.math3.linear.Array2DRowRealMatrix
import nebula.util.Homography
import nebula.util.OpenCVUtil
import nebula.util.KeyPointUtil

import javax.imageio.ImageIO

import java.awt.{ Color, Rectangle }
import java.awt.color.ColorSpace
import java.awt.geom.AffineTransform
import java.awt.image.{ AffineTransformOp, BufferedImage, ColorConvertOp, ConvolveOp, DataBufferInt, Kernel }

import nebula.graveyard._
import nebula.mpie._
import nebula.summary._
import nebula.smallBaseline._
import nebula.util._
import nebula.imageProcessing._
import nebula.wideBaseline._
import nebula._

import scala.Array.{ canBuildFrom, fallbackCanBuildFrom }

import org.opencv.features2d.KeyPoint

import java.awt.image.AffineTransformOp.TYPE_BILINEAR

import breeze.linalg.DenseMatrix

import org.opencv.features2d.{ DMatch, KeyPoint }

import DenseMatrixUtil._

import TestUtil._

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
class TestJSON extends FunSuite {
//  test("test flattenJson on an example") {
//    val wide = WideBaselineExperiment(
//      "wall",
//      2,
//      BoundedPairDetector(
//        BoundedDetector(OpenCVDetector.FAST, 100),
//        10),
//      PatchExtractor(
//        false,
//        false,
//        8,
//        5,
//        "Gray"),
//      Matcher.L1)
//      
//    assert(JSONUtil.flattenJson(wide.toJson) === 
//      "WideBaselineExperiment_D-{OpenCVPairDetector_D-{OpenCVDetector_DT-FAST_MKPO-100}_MKPO-10}_E-{PatchExtractor_BW-5_C-Gray_ET-Rank_NR-false_NS-false_PW-8}_IC-wall_M-L1_OI-2")
//  }
}