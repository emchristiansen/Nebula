package nebula.util

import nebula.graveyard._
import nebula.mpie._
import nebula.summary._
import nebula.smallBaseline._
import nebula.util._
import nebula.util.imageProcessing._
import nebula.wideBaseline._

import java.awt.image.BufferedImage
import java.io.File

import scala.math.{ atan, cos, sin, toDegrees, toRadians }
import scala.math.BigInt.int2bigInt

import org.apache.commons.math3.linear.{ Array2DRowRealMatrix, ArrayRealVector, LUDecomposition, RealVector }
import org.opencv.core.Mat
import org.opencv.features2d.{ DMatch, KeyPoint }
import org.opencv.highgui.Highgui.imread

import javax.imageio.ImageIO
//import net.liftweb.json.{ Formats, JDouble, JField, JInt, JObject, JString, JValue, MappingException, Serializer, TypeInfo }

import spray.json._
import JSONUtil._

///////////////////////////////////////////////////////////

object KeyPointUtil {
  def apply(x: Float, y: Float) = 
    new KeyPoint(x, y, 0, -1, 0, 0, -1)
  
  def isWithinBounds(width: Int, height: Int)(keyPoint: KeyPoint): Boolean = {
    def linearBound(max: Int, pt: Double): Boolean = (pt >= 0) && (pt < max)
    linearBound(width, keyPoint.pt.x) && linearBound(height, keyPoint.pt.y)
  }

  def pairQuality(left: KeyPoint, right: KeyPoint): Double = {
    require(left.response >= 0)
    require(right.response >= 0)
    left.response * right.response
  }
  
  def pairQuality(pair: Tuple2[KeyPoint, KeyPoint]): Double = pairQuality(pair._1, pair._2)
  
  def euclideanDistance(left: KeyPoint, right: KeyPoint): Double = {
    math.sqrt(math.pow(left.pt.x - right.pt.x, 2) + math.pow(left.pt.y - right.pt.y, 2))
  }
  
  def scaleFactor(homography: Homography, xyPoint: RealVector): Double = {
    require(xyPoint.getDimension == 2)

    val plusX = xyPoint.add(new ArrayRealVector(Array(1.0, 0.0)))
    val plusY = xyPoint.add(new ArrayRealVector(Array(0.0, 1.0)))

    val hBase = homography.transform(xyPoint)
    val hPlusX = homography.transform(plusX)
    val hPlusY = homography.transform(plusY)

    val xOffset = hPlusX.subtract(hBase)
    val yOffset = hPlusY.subtract(hBase)

    val parallelepiped = new Array2DRowRealMatrix(
      Array(
        xOffset.toArray,
        yOffset.toArray),
      true)
    (new LUDecomposition(parallelepiped)).getDeterminant
  }

  // Consider the unit vector from |xyPoint| pointing in direction |angle|.
  // Its image under the homography gives the new angle and a scaling factor.
  // The angle is in degrees.
  def locationAndSizeAndAngleUnderHomography(
    homography: Homography,
    basePoint: RealVector,
    size: Double,
    angleInDegrees: Double): Tuple3[RealVector, Double, Double] = {
    val basePointImage = homography.transform(basePoint)

    val tipPointImage = {
      val tipPoint = {
        val angleInRadians = toRadians(angleInDegrees)
        val x = basePoint.getEntry(0) + cos(angleInRadians)
        val y = basePoint.getEntry(1) + sin(angleInRadians)
        new ArrayRealVector(Array(x, y))
      }
      homography.transform(tipPoint)
    }

    val delta = tipPointImage.subtract(basePointImage)

    val imageAngle = atan(delta.getEntry(1) / delta.getEntry(0))
    (basePointImage, scaleFactor(homography, basePoint) * size, toDegrees(imageAngle))
  }

  // A proper warping of a keypoint between images, mapping over the
  // size and angle under the homography.
  // TODO: What to do with |octave|?
  // TODO: Add such a function to OpenCV if it doesn't have one.
  def transform(homography: Homography)(keyPoint: KeyPoint): KeyPoint = {
    val xyPoint = new ArrayRealVector(Array(
      keyPoint.pt.x,
      keyPoint.pt.y))

    val (imagePoint, imageSize, imageAngle) = locationAndSizeAndAngleUnderHomography(
      homography,
      xyPoint,
      keyPoint.size,
      keyPoint.angle)

    // TODO: This is probably broken.
    // 0, 0, 0, -1, 0, 0, -1
    new KeyPoint(
      imagePoint.getEntry(0).toFloat,
      imagePoint.getEntry(1).toFloat,
      keyPoint.size,
      keyPoint.angle,
      keyPoint.response,
      keyPoint.octave,
      keyPoint.class_id)

    //    new KeyPoint(
    //      imagePoint.getEntry(0).toFloat,
    //      imagePoint.getEntry(1).toFloat,
    //      imageSize.toFloat,
    //      imageAngle.toFloat,
    //      keyPoint.response,
    //      keyPoint.octave,
    //      keyPoint.class_id)
  }
}

///////////////////////////////////////////////////////////

object OpenCVUtil {
  def bufferedImageToMat(image: BufferedImage): Mat = {
    // TODO: Figure out how to do this without IO.
    val file = File.createTempFile("bufferedImageToCvMat", ".bmp")
    ImageIO.write(image, "bmp", file)
    val mat = imread(file.toString)
    file.delete
    assert(mat != null)
    mat
  }
}

/////////////////////////////////////////////////////////////
//
//class DMatchSerializer extends Serializer[DMatch] {
//  private val DMatchClass = classOf[DMatch]
//
//  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), DMatch] = {
//    case (TypeInfo(DMatchClass, _), json) => json match {
//      case JObject(
//        JField("jsonClass", JString("DMatch")) ::
//          JField("queryIdx", JInt(queryIdx)) ::
//          JField("trainIdx", JInt(trainIdx)) ::
//          JField("distance", JDouble(distance)) :: Nil) =>
//        new DMatch(queryIdx.toInt, trainIdx.toInt, distance.toFloat)
//      case x => throw new MappingException("Can't convert " + x + " to DMatch")
//    }
//  }
//
//  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
//    case x: DMatch =>
//      JObject(
//        JField("jsonClass", JString("DMatch")) ::
//          JField("queryIdx", JInt(x.queryIdx)) ::
//          JField("trainIdx", JInt(x.trainIdx)) ::
//          JField("distance", JDouble(x.distance)) :: Nil)
//  }
//}

///////////////////////////////////////////////////////////

object DMatchJsonProtocol extends DefaultJsonProtocol {
  implicit object DMatchJsonProtocol extends RootJsonFormat[DMatch] {
    override def write(self: DMatch) = JsObject(
      "queryIdx" -> JsNumber(self.queryIdx),
      "trainIdx" -> JsNumber(self.trainIdx),
      "distance" -> JsNumber(self.distance),
      "scalaClass" -> JsString("DMatch"))
    override def read(value: JsValue) =
      value.asJsObject.getFields("queryIdx", "trainIdx", "distance", "scalaClass") match {
        case Seq(JsNumber(queryIdx), JsNumber(trainIdx), JsNumber(distance), JsString(scalaClass)) =>
          new DMatch(queryIdx.toInt, trainIdx.toInt, distance.toFloat)
      }
  }
}
