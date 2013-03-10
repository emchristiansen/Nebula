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
   * MachineError for any container type that can be viewed as a Seq.
   */
  implicit def seq2MachineError[A <% MachineError[A], C[_]](implicit cToSeq: C[A] => Seq[A]): MachineErrorPimp[C[A]] =
    apply((left: C[A], right: C[A]) =>
      if (left.size != right.size) Double.PositiveInfinity
      else {
        val errors = (left zip right) map {
          case (l, r) => l machineError r
        }
        errors.max
      })  
}

object MachineError extends MachineErrorImplicits