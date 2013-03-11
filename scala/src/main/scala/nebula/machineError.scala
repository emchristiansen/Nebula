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
 * Anything Seq-like on which it is valid to compute machine error.
 * The purpose of this trait is to avoid polluting the implicit
 * path with views from weird structs to Seq.
 */
trait MachineErrorSeq[A] {
  def seq: Seq[A]
}

trait MachineErrorSeqImplicitConstructors {
  implicit def seqToMachineErrorSeq[A](seq: Seq[A]): MachineErrorSeq[A] =
    MachineErrorSeq(seq)

  implicit def denseMatrixToMachineErrorSeq[A](
    matrix: DenseMatrix[A]): MachineErrorSeq[A] =
    MachineErrorSeq(matrix.copy.data)
}

object MachineErrorSeq extends MachineErrorSeqImplicitConstructors {
  def apply[A](self: Seq[A]): MachineErrorSeq[A] = new MachineErrorSeq[A] {
    override def seq = self
  }

  implicit def machineErrorSeqToSeq[A](self: MachineErrorSeq[A]): Seq[A] =
    self.seq
}

///////////////////////////////////////////////////////////

/**
 * A measure of the difference between two objects when that difference is
 * caused by machine error, eg floating point error.
 * This will normally be defined as the maximum pairwise difference between
 * the objects, but I'm avoiding calling this the l-infinity norm because I
 * may want to define it for weird objects.
 */
trait MachineError[A] {
  def machineError: (A) => Double
}

trait MachineErrorImplicits {
  type MachineErrorPimp[A] = A => MachineError[A]

  def apply[A](error: (A, A) => Double) = (left: A) => new MachineError[A] {
    override def machineError = (right: A) => error(left, right)
  }

  implicit def double2MachineError[A <% Double]: MachineErrorPimp[A] =
    apply((left: A, right: A) => (left - right).abs)

  implicit def spireComplex2MachineError[A <% Double: SpireNumeric]: MachineErrorPimp[SpireComplex[A]] = {
    apply((left: SpireComplex[A], right: SpireComplex[A]) => (left - right).abs)
  }

  /**
   * Workaround for Scala's limited implicit search.
   */
  // TODO
  implicit def spireComplex2MachineError: MachineErrorPimp[SpireComplex[Double]] =
    spireComplex2MachineError[Double]

  implicit def apacheComplex2MachineError: MachineErrorPimp[ApacheComplex] =
    apply((left: ApacheComplex, right: ApacheComplex) =>
      ComplexUtil.apacheToSpire(left) machineError ComplexUtil.apacheToSpire(right))

  implicit def breezeComplex2MachineError: MachineErrorPimp[BreezeComplex] =
    apply((left: BreezeComplex, right: BreezeComplex) =>
      ComplexUtil.breezeToSpire(left) machineError ComplexUtil.breezeToSpire(right))
  
  /**
   * MachineError for any container type that can be viewed as a MachineErrorSeq.
   */     
  implicit def machineErrorSeq2MachineError2[
    C <% MachineErrorSeq[A], A <% MachineError[A]]: MachineErrorPimp[C] =
    apply((left: C, right: C) =>
      if (left.seq.size != right.seq.size) Double.PositiveInfinity
      else {
        val errors = (left.seq zip right.seq) map {
          case (l, r) => l machineError r
        }
        errors.max
      })
}

object MachineError extends MachineErrorImplicits