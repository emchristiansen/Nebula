package nebula

import java.awt.image.BufferedImage

import scala.math.BigInt.int2bigInt

import org.apache.commons.math3.linear.{Array2DRowRealMatrix, ArrayRealVector, LUDecomposition, RealVector}
import org.opencv.core.Mat
import org.opencv.features2d.{DMatch, KeyPoint}
import org.opencv.highgui.Highgui.imread

import javax.imageio.ImageIO
import net.liftweb.json.{Formats, JDouble, JField, JInt, JObject, JString, JValue, MappingException, Serializer, TypeInfo}

object KeyPointUtil {
  def isWithinBounds(width: Int, height: Int)(keyPoint: KeyPoint): Boolean = {
    def linearBound(max: Int, pt: Double): Boolean = (pt >= 0) && (pt < max)
    linearBound(width, keyPoint.pt.x) && linearBound(height, keyPoint.pt.y)
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
      keyPoint.pt.x,
      keyPoint.pt.y))

    val size = scaleFactor(homography, xyPoint) * keyPoint.size

    val angle = {
      // TODO: Fix this
      keyPoint.angle
    }

    val xyVector = homography.transform(new ArrayRealVector(Array(
      keyPoint.pt.x,
      keyPoint.pt.y)))

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
  def bufferedImageToMat(image: BufferedImage): Mat = {
    // TODO: Figure out how to do this without IO.
    val file = IO.createTempFile("bufferedImageToCvMat", ".bmp")
    ImageIO.write(image, "bmp", file)
    val mat = imread(file.toString)
    assert(mat != null)
    mat
  }

  // TODO: Delete this crap. 
  
  //  def cvMatStringToSeq(matString: String): Seq[Double] = {
  //    val tokens = matString.replace("[", "").replace("]", "").split(",")
  //    val noWhitespace = tokens.map(_.replace(" ", ""))
  //    tokens.map(_.toDouble)
  //  }
  //
  //  // We're going to get the string representation of the |CvMat|,
  //  // and parse it to get out its values.
  //  // Yes, the JavaCV interface really is this broken.
  //  def cvMatToSeq(mat: CvMat): Seq[Double] = {
  //    assert(mat.rows == 1)
  //    assert(mat.cols > 0)
  //    cvMatStringToSeq(mat.toString)
  //  }
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
