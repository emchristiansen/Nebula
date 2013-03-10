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

trait IsNearMethods {
  def assertNear[A <% MachineError[A]](
    left: => A,
    right: => A)(implicit threshold: Epsilon): Unit = {
    Predef.assert(
      (left machineError right) <= threshold,
      s"\nleft: ${left}\nright: ${right}")    
  }
}

object IsNear extends IsNearMethods

trait Near extends IsNearMethods {
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