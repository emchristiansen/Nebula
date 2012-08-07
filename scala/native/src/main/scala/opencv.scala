package nebula

import java.io.File

import com.googlecode.javacv.cpp.opencv_contrib._
import com.googlecode.javacv.cpp.opencv_core._
import com.googlecode.javacv.cpp.opencv_features2d._

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
    new KeyPoint(x, y, defaultSize, defaultAngle, defaultResponse, defaultOctave, defaultClassID)
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

  def keyPointsToList(keyPoints: KeyPoint): List[KeyPoint] = {
    for (index <- 0 until keyPoints.capacity toList) yield {
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

  def listToKeyPoints(keyPointsList: List[KeyPoint]): KeyPoint = {
    val keyPoints = new KeyPoint(keyPointsList.size)
    for ((keyPoint, index) <- keyPointsList.zipWithIndex) {
      setKeyPointAt(keyPoints, index, keyPoint)
    }
    keyPoints.position(0)
    keyPoints
  }
}
