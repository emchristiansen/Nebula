package nebula

import java.awt.image.BufferedImage

import scala.Array.canBuildFrom
import scala.Array.fallbackCanBuildFrom
import scala.math.BigInt.int2bigInt

import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.apache.commons.math3.linear.ArrayRealVector
import org.apache.commons.math3.linear.LUDecomposition
import org.apache.commons.math3.linear.RealVector

import com.googlecode.javacv.cpp.opencv_core.CvMat
import com.googlecode.javacv.cpp.opencv_features2d.DMatch
import com.googlecode.javacv.cpp.opencv_features2d.KeyPoint
import com.googlecode.javacv.cpp.opencv_highgui.cvLoadImageM

import javax.imageio.ImageIO
import net.liftweb.json.Formats
import net.liftweb.json.JDouble
import net.liftweb.json.JField
import net.liftweb.json.JInt
import net.liftweb.json.JObject
import net.liftweb.json.JString
import net.liftweb.json.JValue
import net.liftweb.json.MappingException
import net.liftweb.json.Serializer
import net.liftweb.json.TypeInfo

// Warning: A KeyPoint is both a single key point and a collection of
// key points. Terrible design, I know, but not mine. The methods that
// operate on KeyPoints largely rely on side effects.
object KeyPointUtil {
  val defaultSize = 0
  val defaultAngle = -1
  val defaultResponse = 0
  val defaultOctave = 0
  val defaultClassID = -1

  def withDefaults(x: Float, y: Float): KeyPoint = {
    new KeyPoint(
      x, 
      y, 
      defaultSize, 
      defaultAngle, 
      defaultResponse, 
      defaultOctave, 
      defaultClassID)
  }

  def isWithinBounds(width: Int, height: Int)(keyPoint: KeyPoint): Boolean = {
    def linearBound(max: Int, pt: Double): Boolean = (pt >= 0) && (pt < max)
    linearBound(width, keyPoint.pt_x) && linearBound(height, keyPoint.pt_y)
  }

  def keyPointAt(keyPoints: KeyPoint, index: Int): KeyPoint = {
    keyPoints.position(index)
    new KeyPoint(keyPoints.pt, 
		 keyPoints.size, 
		 keyPoints.angle, 
		 keyPoints.response, 
		 keyPoints.octave, 
		 keyPoints.class_id)
  }

  def keyPointsToSeq(keyPoints: KeyPoint): Seq[KeyPoint] = {
    for (index <- 0 until keyPoints.capacity toSeq) yield {
      keyPointAt(keyPoints, index)
    }
  }

  def setKeyPointAt(keyPoints: KeyPoint, index: Int, keyPoint: KeyPoint) {
    keyPoints.position(index)
    keyPoints.pt(keyPoint.pt)
    keyPoints.size(keyPoint.size)
    keyPoints.angle(keyPoint.angle)
    keyPoints.response(keyPoint.response)
    keyPoints.octave(keyPoint.octave)
    keyPoints.class_id(keyPoint.class_id)
  }

  def listToKeyPoints(keyPointsSeq: Seq[KeyPoint]): KeyPoint = {
    val keyPoints = new KeyPoint(keyPointsSeq.size)
    for ((keyPoint, index) <- keyPointsSeq.zipWithIndex) {
      setKeyPointAt(keyPoints, index, keyPoint)
    }
    keyPoints.position(0)
    keyPoints
  }
  
  def toString(keyPoint: KeyPoint): String = {
    "KeyPoint(%s, %s, %s, %s, %s, %s, %s)".format(
      keyPoint.pt_x,
      keyPoint.pt_y,
      keyPoint.size,
      keyPoint.angle,
      keyPoint.response,
      keyPoint.octave,
      keyPoint.class_id)
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

  // A proper warping of a keypoint between images, mapping over the
  // size and angle under the homography.
  // TODO: What to do with |octave|?
  // TODO: Add such a function to OpenCV if it doesn't have one.
  def transform(homography: Homography)(keyPoint: KeyPoint): KeyPoint = {
    val xyPoint = new ArrayRealVector(Array(
      keyPoint.pt_x.toDouble, 
      keyPoint.pt_y.toDouble))

    val size = scaleFactor(homography, xyPoint) * keyPoint.size

    val angle = {
      // TODO: Are we using radians or degrees?
      assert(keyPoint.angle == -1)
      -1
    }

    val xyVector = homography.transform(new ArrayRealVector(Array(
      keyPoint.pt_x.toDouble,
      keyPoint.pt_y.toDouble)))

    new KeyPoint(
      xyVector.getEntry(0).toFloat,
      xyVector.getEntry(1).toFloat,
      size.toFloat,
      angle,
      keyPoint.response,
      keyPoint.octave,
      keyPoint.class_id)
  }
}

object OpenCVUtil {
  def bufferedImageToCvMat(image: BufferedImage): CvMat = {
    // TODO: Figure out how to do this without IO.
    val file = IO.createTempFile("bufferedImageToCvMat", ".bmp")
    ImageIO.write(image, "bmp", file)
    val matImage = cvLoadImageM(file.toString)
    assert(matImage != null)
    matImage
  }

  def cvMatStringToSeq(matString: String): Seq[Double] = {
    val tokens = matString.replace("[", "").replace("]", "").split(",")
    val noWhitespace = tokens.map(_.replace(" ", ""))
    tokens.map(_.toDouble)
  }

  // We're going to get the string representation of the |CvMat|,
  // and parse it to get out its values.
  // Yes, the JavaCV interface really is this broken.
  def cvMatToSeq(mat: CvMat): Seq[Double] = {
    assert(mat.rows == 1)
    assert(mat.cols > 0)
    cvMatStringToSeq(mat.toString)
  }
}

class DMatchSerializer extends Serializer[DMatch] {
  private val DMatchClass = classOf[DMatch]

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), DMatch] = {
    case (TypeInfo(DMatchClass, _), json) => json match {
      case JObject(
	JField("jsonClass", JString("DMatch")) ::
	JField("queryIdx", JInt(queryIdx)) :: 
        JField("trainIdx", JInt(trainIdx)) :: 
        JField("distance", JDouble(distance)) :: Nil) =>
        new DMatch(queryIdx.toInt, trainIdx.toInt, distance.toFloat)
      case x => throw new MappingException("Can't convert " + x + " to DMatch")
    }
  }

  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case x: DMatch =>
      JObject(
	JField("jsonClass", JString("DMatch")) ::
	JField("queryIdx", JInt(x.queryIdx)) :: 
        JField("trainIdx", JInt(x.trainIdx)) :: 
	JField("distance", JDouble(x.distance)) :: Nil)
  }
}
