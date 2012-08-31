package nebula

import java.awt.geom.AffineTransform
import java.io.File

import scala.Array.canBuildFrom

import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.apache.commons.math3.linear.ArrayRealVector
import org.apache.commons.math3.linear.LUDecomposition
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.linear.RealMatrix
import org.apache.commons.math3.linear.RealVector
import org.apache.commons.math3.linear.SingularValueDecomposition

case class Homography(matrix: RealMatrix) {
  require(matrix.getRowDimension == 3)
  require(matrix.getColumnDimension == 3)

  def transform(in: RealVector): RealVector = {
    require(in.getDimension == 2)
    val inHomogeneous = Geometry.homogenize(in)
    val outHomogeneous = matrix.operate(inHomogeneous)
    Geometry.dehomogenize(outHomogeneous)
  }
}

object Homography {
  def fromFile(file: File): Homography = {
    val lines = Util.linesFromFile(file)
    val values = lines.map(_.split("[ \t]").filter(_.size > 0).map(_.toDouble).toArray).toArray
    Homography(new Array2DRowRealMatrix(values))
  }
}


object Geometry {
  def homogenize(inhomogeneous: RealVector): RealVector =
    inhomogeneous.append(new ArrayRealVector(Array(1.0)))

  def dehomogenize(homogeneous: RealVector): RealVector = {
    val homogeneousElement = homogeneous.getEntry(homogeneous.getDimension - 1)
    require(homogeneousElement != 0)
    val normalized = homogeneous.mapMultiply(1.0 / homogeneousElement)
    normalized.getSubVector(0, homogeneous.getDimension - 1)
  }

  def fitAffine(source: List[Point2D], target: List[Point2D]): AffineTransform = {
    assert(source.size == target.size)
    assert(source.size >= 3)
    
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
    
    assert(transformation.getRowDimension == 2)
    assert(transformation.getColumnDimension == 3)
    
    new AffineTransform(transformation.getData.transpose.flatten)
  }

  // TODO: Unit test this beast.
  def fitSimilarity(xsList: List[Tuple2[Double, Double]], ysList: List[Tuple2[Double, Double]]): List[List[Double]] = {
    // Calcuates similarity using method of 
    // "Least-Squares Estimation of Transformation Parameters Between To Point Patterns" by Umeyama.
    // Returns 3x3 homogeneous transformation matrix.

    assert(xsList.size == ysList.size && xsList.size >= 2)

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
