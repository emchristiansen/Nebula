package nebula.util

import java.awt.geom.AffineTransform
import java.awt.image.AffineTransformOp
import java.awt.image.BufferedImage
import java.io.File

import scala.Array.canBuildFrom
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION

import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.apache.commons.math3.linear.ArrayRealVector
import org.apache.commons.math3.linear.LUDecomposition
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.linear.RealMatrix
import org.apache.commons.math3.linear.RealVector
import org.apache.commons.math3.linear.SingularValueDecomposition
import org.opencv.features2d.KeyPoint

import nebula.graveyard.Point2D
import nebula._
import MathUtil._
import org.apache.commons.math3.linear._

///////////////////////////////////////////////////////////

// TODO: Use Breeze vectors throughout entire file.
case class Homography(matrix: RealMatrix) {
  requirey(matrix.getRowDimension == 3)
  requirey(matrix.getColumnDimension == 3)

  def transform(in: RealVector): RealVector = {
    requirey(in.getDimension == 2)
    val inHomogeneous = Geometry.homogenize(in)
    val outHomogeneous = matrix.operate(inHomogeneous)
    Geometry.dehomogenize(outHomogeneous)
  }

  def transformXYOnly(in: KeyPoint): KeyPoint = {
    val inVector = new ArrayRealVector(Array(in.pt.x, in.pt.y))
    val outVector = transform(inVector)
    new KeyPoint(
      outVector.getEntry(0).toFloat,
      outVector.getEntry(1).toFloat,
      0)
  }

  def transform(left: KeyPoint): KeyPoint = {
    val leftRay = Geometry.getRayPoint(left)

    val right = transformXYOnly(left)
    val rightRay = transformXYOnly(leftRay)

    def getScaleAndRotation(base: KeyPoint, tip: KeyPoint): ScaleAndRotation = {
      val offsetX = tip.pt.x - base.pt.x
      val offsetY = tip.pt.y - base.pt.y

      val scale = MathUtil.l2Norm(Array(offsetX, offsetY))
      val rotation = math.atan2(offsetY, offsetX)
      ScaleAndRotation(scale, rotation mod (2 * math.Pi))
    }

    // We use the difference in scale and rotation to determine the
    // scale and rotation of the output KeyPoint.
    val leftSimilarity = getScaleAndRotation(left, leftRay)
    val rightSimilarity = getScaleAndRotation(right, rightRay)

    val rightSize =
      left.size * rightSimilarity.scale / leftSimilarity.scale
    new KeyPoint(
      right.pt.x.toFloat,
      right.pt.y.toFloat,
      rightSize.toFloat,
      rightSimilarity.rotation.toFloat)
  }

}

object Homography {
  def fromFile(file: File): Homography = {
    val lines = Util.linesFromFile(file)
    val values = lines.map(_.split("[ \t]").filter(_.size > 0).map(_.toDouble).toArray).toArray
    Homography(new Array2DRowRealMatrix(values))
  }
}

///////////////////////////////////////////////////////////

case class ScaleAndRotation(scale: Double, rotation: Double) {
  asserty(scale > 0)
  asserty(rotation >= 0)
  asserty(rotation < 2 * math.Pi)
}

object Geometry {
  /**
   * The angle and scale is represented as an extra KeyPoint, located
   * a distance proportional to the KeyPoint size along the ray
   * given by the KeyPoint angle.
   */
  def getRayPoint(keyPoint: KeyPoint): KeyPoint = {
    // Used to ensure locality. Smaller is better, but too small
    // means precision problems.
    val epsilon = 0.1
    val offset = keyPoint.size * epsilon

    // This part is tricky, because we need to be sure the coordinate
    // systems are consistent.
    val x = keyPoint.pt.x + offset * math.cos(keyPoint.angle)
    val y = keyPoint.pt.y + offset * math.sin(keyPoint.angle)

    new KeyPoint(x.toFloat, y.toFloat, 0)
  }

  //  def scaleAndRotationOfHomographyAtPoint(
  //    homography: Homography,
  //    point: KeyPoint): ScaleAndRotation = {
  //    
  //  }

  /**
   * Homography from four points.
   */
  def fitHomographyFromFourTranslations(
    leftKeyPoints: Seq[KeyPoint],
    rightKeyPoints: Seq[KeyPoint]): Homography = {
    def validate(keyPoints: Seq[KeyPoint]) {
      requirey(keyPoints.size == 4)
    }
    validate(leftKeyPoints)
    validate(rightKeyPoints)

    // From Hartley and Zisserman "Multiple View Geometry in Computer Vision" p89.
    def keyPointPairToConstraint(
      leftKeyPoint: KeyPoint,
      rightKeyPoint: KeyPoint): RealMatrix = {
      val x: Double = leftKeyPoint.pt.x
      val y: Double = leftKeyPoint.pt.y
      val w: Double = 1

      val xp: Double = rightKeyPoint.pt.x
      val yp: Double = rightKeyPoint.pt.y
      val wp: Double = 1

      val data = Array[Double](
        0, 0, 0, -wp * x, -wp * y, -wp * w, yp * x, yp * y, yp * w,
        wp * x, wp * y, wp * w, 0, 0, 0, -xp * x, -xp * y, -xp * w).grouped(9).toArray

      val matrix = new Array2DRowRealMatrix(data)
      asserty(matrix.getRowDimension == 2)
      asserty(matrix.getColumnDimension == 9)
      matrix
    }

    val constraints =
      (leftKeyPoints zip rightKeyPoints) map {
        case (left, right) => keyPointPairToConstraint(left, right)
      }

    def concatenateVertical(left: RealMatrix, right: RealMatrix): RealMatrix = {
      new Array2DRowRealMatrix(left.getData ++ right.getData)
    }

    val constraint = constraints.reduce(concatenateVertical)
    asserty(constraint.getRowDimension == 8)
    asserty(constraint.getColumnDimension == 9)

    val qr = new QRDecomposition(constraint.transpose)
    val q = qr.getQ
    // We want the last column of Q.
    val data = for (row <- 0 until 9) yield q.getEntry(row, 8) / q.getEntry(8, 8)

    val matrix = new Array2DRowRealMatrix(data.toArray.grouped(3).toArray)
    val homography = Homography(matrix)

    //    // From Hartley and Zisserman "Multiple View Geometry in Computer Vision" p91,
    //    // inhomogeneous solution.
    //    def keyPointPairToConstraintMatrices(
    //      leftKeyPoint: KeyPoint,
    //      rightKeyPoint: KeyPoint): (RealMatrix, RealMatrix) = {
    //      val x: Double = leftKeyPoint.pt.x
    //      val y: Double = leftKeyPoint.pt.y
    //      val w: Double = 1
    //
    //      val xp: Double = rightKeyPoint.pt.x
    //      val yp: Double = rightKeyPoint.pt.y
    //      val wp: Double = 1
    //
    //      val left = {
    //        val data = Array[Double](
    //          0, 0, 0, -x * wp, -y * wp, -w * wp, x * yp, y * yp,
    //          x * wp, y * wp, w * wp, 0, 0, 0, -x * xp, -y * xp).grouped(8).toArray
    //        val matrix = new Array2DRowRealMatrix(data)
    //        asserty(matrix.getRowDimension == 2)
    //        asserty(matrix.getColumnDimension == 8)
    //        matrix
    //      }
    //
    //      val right = {
    //        val data = Array[Double](
    //          -w * yp,
    //          w * xp).grouped(1).toArray
    //        val matrix = new Array2DRowRealMatrix(data)
    //        asserty(matrix.getRowDimension == 2)
    //        asserty(matrix.getColumnDimension == 1)
    //        matrix
    //      }
    //
    //      (left, right)
    //    }
    //
    //    val constraintPairs =
    //      (leftKeyPoints zip rightKeyPoints) map {
    //        case (left, right) => keyPointPairToConstraintMatrices(left, right)
    //      }
    //
    //    def concatenateVertical(left: RealMatrix, right: RealMatrix): RealMatrix = {
    //      new Array2DRowRealMatrix(left.getData ++ right.getData)
    //    }
    //
    //    val leftConstraint = constraintPairs.map(_._1).reduce(concatenateVertical)
    //    asserty(leftConstraint.getRowDimension == 8)
    //    asserty(leftConstraint.getColumnDimension == 8)
    //
    //    val rightConstraint = constraintPairs.map(_._2).reduce(concatenateVertical)
    //    asserty(rightConstraint.getRowDimension == 8)
    //    asserty(rightConstraint.getColumnDimension == 1)
    //
    //    val solver = new SingularValueDecomposition(leftConstraint).getSolver
    //    val hStacked = solver.solve(rightConstraint)
    //    
    //    val homography = {
    //      val data: Array[Double] = hStacked.getData.flatten.toArray ++ Array(1.0)
    //      val matrix = new Array2DRowRealMatrix(data.grouped(3).toArray)
    //      Homography(matrix)
    //    }

//    for ((leftKeyPoint, rightKeyPoint) <- leftKeyPoints zip rightKeyPoints) {
//      val projected = homography.transformXYOnly(leftKeyPoint)
//      implicit def epsilon = Epsilon(1.0)
//      assertNear(projected.pt.x, rightKeyPoint.pt.x)
//      assertNear(projected.pt.y, rightKeyPoint.pt.y)
//    }

    homography
  }

  /**
   * Two correspondences with know scale and angle change are used
   * to fit a homography.
   * The homography is from the left to the right.
   */
  def fitHomographyFromTwoSimilarityCorrespondences(
    left: Seq[KeyPoint],
    right: Seq[KeyPoint]): Homography = {
    def validate(keyPoints: Seq[KeyPoint]) = {
      asserty(keyPoints.size == 2)
      keyPoints foreach { keyPoint =>
        asserty(keyPoint.size > 0)
        asserty(keyPoint.angle >= 0)
        asserty(keyPoint.angle < 2 * math.Pi)
      }
    }
    validate(left)
    validate(right)

    def addRayPoint(keyPoint: KeyPoint): Seq[KeyPoint] = {
      Seq(keyPoint, getRayPoint(keyPoint))
    }

    val leftFour = left.flatMap(addRayPoint)
    val rightFour = right.flatMap(addRayPoint)

    fitHomographyFromFourTranslations(leftFour, rightFour)
  }

  def homogenize(inhomogeneous: RealVector): RealVector =
    inhomogeneous.append(new ArrayRealVector(Array(1.0)))

  def dehomogenize(homogeneous: RealVector): RealVector = {
    val homogeneousElement = homogeneous.getEntry(homogeneous.getDimension - 1)
    requirey(homogeneousElement != 0)
    val normalized = homogeneous.mapMultiply(1.0 / homogeneousElement)
    normalized.getSubVector(0, homogeneous.getDimension - 1)
  }

  def fitAffine(source: List[Point2D], target: List[Point2D]): AffineTransform = {
    asserty(source.size == target.size)
    asserty(source.size >= 3)

    val lhs = {
      val data = target.map(_.toList).transpose.map(_.toArray).toArray
      new Array2DRowRealMatrix(data).transpose
    }

    val rhs = {
      val data = source.map(_.toListHomogeneous).transpose.map(_.toArray).toArray
      new Array2DRowRealMatrix(data).transpose
    }

    val solver = new SingularValueDecomposition(rhs).getSolver
    val transformation = solver.solve(lhs).transpose

    asserty(transformation.getRowDimension == 2)
    asserty(transformation.getColumnDimension == 3)

    new AffineTransform(transformation.getData.transpose.flatten)
  }

  // TODO: Unit test this beast.
  def fitSimilarity(xsList: List[Tuple2[Double, Double]], ysList: List[Tuple2[Double, Double]]): List[List[Double]] = {
    // Calcuates similarity using method of 
    // "Least-Squares Estimation of Transformation Parameters Between To Point Patterns" by Umeyama.
    // Returns 3x3 homogeneous transformation matrix.

    asserty(xsList.size == ysList.size && xsList.size >= 2)

    def vectorize(v: Tuple2[Double, Double]): RealVector = {
      val (v1, v2) = v
      new ArrayRealVector(Array(v1, v2))
    }

    val xs = xsList.map(vectorize)
    val ys = ysList.map(vectorize)

    def mean(vs: List[RealVector]): RealVector = {
      vs.reduce((l, r) => l.add(r)).mapMultiply(1.0 / vs.size.toDouble)
    }

    val muX = mean(xs)
    val muY = mean(ys)

    val centeredXs = xs.map(_.subtract(muX))
    val centeredYs = ys.map(_.subtract(muY))

    def covariance(v1s: List[RealVector], v2s: List[RealVector]): RealMatrix = {
      // Assume entries are centered.
      val outers = for ((v1, v2) <- v1s.zip(v2s)) yield v1.outerProduct(v2)
      outers.reduce((l, r) => l.add(r)).scalarMultiply(1.0 / v1s.size.toDouble)
    }

    val sigmaXSquared = covariance(centeredXs, centeredXs).getTrace
    val sigmaYSquared = covariance(centeredYs, centeredYs).getTrace
    val SigmaXY = covariance(centeredXs, centeredYs)

    val S = {
      val S = MatrixUtils.createRealIdentityMatrix(2)
      val determinant = new LUDecomposition(SigmaXY, 0).getDeterminant
      if (determinant < 0) {
        S.setEntry(1, 1, -1)
      }
      S
    }

    val svd = new SingularValueDecomposition(SigmaXY)

    val R = svd.getU.multiply(S.multiply(svd.getVT))
    val c = 1 / sigmaXSquared * svd.getS.multiply(S).getTrace
    val t = {
      val projected = R.operate(muX).mapMultiply(c)
      muY.subtract(projected)
    }

    val cR = R.scalarMultiply(c)

    val row0 = List(cR.getEntry(0, 0), cR.getEntry(0, 1), t.getEntry(0))
    val row1 = List(cR.getEntry(1, 0), cR.getEntry(1, 1), t.getEntry(1))
    val row2 = List(0.0, 0.0, 1.0)
    List(row0, row1, row2)
  }
}
