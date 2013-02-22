package nebula

import scala.reflect.runtime._
import scala.reflect._
import spray.json._
import breeze.linalg.DenseMatrix
import breeze.math.Complex
import spire.algebra._
import spire.math._
import spire.implicits._
import nebula.util._

///////////////////////////////////////////////////////////

/**
 * Represents a small positive number.
 */
case class Epsilon(value: Double) {
  requirey(value > 0)
}

object Epsilon {
  implicit def epsilon2Double(self: Epsilon): Double = self.value
}

/**
 * Trait for deciding whether two non-identical objects are at least
 * near each other.
 */
trait IsNear[A] {
  def isNear(that: A)(implicit threshold: Epsilon): Boolean
}

trait IsNearImplicits {
  implicit class Double2IsNear[A <% Double](self: A) extends IsNear[A] {
    override def isNear(other: A)(implicit threshold: Epsilon) =
      (self - other).abs <= threshold
  }

  implicit class SpireComplex2IsNear[A <% IsNear[A]: SpireNumeric](
    self: SpireComplex[A]) extends IsNear[SpireComplex[A]] {
    override def isNear(other: SpireComplex[A])(implicit threshold: Epsilon) =
      (self - other).abs <= threshold
  }

  implicit class ApacheComplex2IsNear(
    self: ApacheComplex) extends IsNear[ApacheComplex] {
    override def isNear(other: ApacheComplex)(implicit threshold: Epsilon) =
      ComplexUtil.apacheToSpire(self).isNear(ComplexUtil.apacheToSpire(other))
  }
  
  implicit class BreezeComplex2IsNear(
      self: BreezeComplex) extends IsNear[BreezeComplex] {
    override def isNear(other: BreezeComplex)(implicit threshold: Epsilon) =
      ComplexUtil.breezeToSpire(self).isNear(ComplexUtil.breezeToSpire(other))
  }

  // TODO: Currently the more general code (see below) crashes the compiler.
  implicit class IndexedSeq2IsNear[A <% IsNear[A]](
    self: IndexedSeq[A]) extends IsNear[IndexedSeq[A]] {
    override def isNear(other: IndexedSeq[A])(implicit threshold: Epsilon) = {
      self.size == other.size && (self.zip(other).count {
        case (left, right) => left.isNear(right)
      }) == self.size
    }
  }

  implicit class DenseMatrix2IsNear[A <% IsNear[A]](
    self: DenseMatrix[A]) extends IsNear[DenseMatrix[A]] {
    override def isNear(other: DenseMatrix[A])(implicit threshold: Epsilon) = {
      self.rows == other.rows &&
        self.cols == other.cols &&
        (self.data.toIndexedSeq.zip(other.data.toIndexedSeq).count {
          case (left, right) => left.isNear(right)
        }) == self.size
    }
  }

  // This code crashes the compiler.
  // TODO: Uncomment
  //  implicit class Seq2IsNear[C[_] <: Iterable[_], A <% IsNear[A]](self: C[A]) extends IsNear[C[A]] {
  //    override def isNear(other: C[A])(implicit threshold: Epsilon) = {
  //      self.size == other.size && (self.zip(other).count {
  //        case (left, right) => left.isNear(right)
  //      }) == self.size
  //    }
  //  }  
}

trait IsNearMethods {
  def assertNear[A <% IsNear[A]](
    left: => A,
    right: => A)(implicit threshold: Epsilon): Unit = {
    Predef.assert(
      left.isNear(right),
      s"\nleft: ${left}\nright: ${right}")
  }
}

object IsNear extends IsNearImplicits with IsNearMethods

trait Near extends IsNearImplicits with IsNearMethods {
  implicit val epsilon = Epsilon(0.0001)

  /**
   * Checks the ratio of two positive numbers is close to 1.
   */
  def assertRelativelyNear(maxRatio: Double)(left: Double, right: Double) {
    requirey(maxRatio >= 1)
    requirey(left > 0)
    requirey(right > 0)

    //    asserty(left / right <= maxRatio, s"${left} / ${right} = ${left / right} > ${maxRatio}")
    //    asserty(right / left <= maxRatio, s"${right} / ${left} = ${right / left} > ${maxRatio}")
    asserty(left / right <= maxRatio)
    asserty(right / left <= maxRatio)
  }
}