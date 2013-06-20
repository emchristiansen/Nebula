package nebula.util

import nebula._
import org.apache.commons.math3
import spire.algebra._
import spire.math._
import spire.implicits._

///////////////////////////////////

/**
 * Functions for converting between Complex implementations and also between
 * real types and complex types.
 */
object ComplexUtil {
  def apacheToSpire(self: ApacheComplex): SpireComplex[Double] =
    new SpireComplex(self.getReal, self.getImaginary)

  def spireToApache[A <% Double](self: SpireComplex[A]): ApacheComplex =
    new ApacheComplex(self.real, self.imag)

  def breezeToSpire(self: BreezeComplex): SpireComplex[Double] =
    new SpireComplex(self.real, self.imag)

  def spireToBreeze[A <% Double](self: SpireComplex[A]): BreezeComplex =
    new BreezeComplex(self.real, self.imag)

  def apacheToBreeze = (spireToBreeze[Double] _) compose apacheToSpire

  def breezeToApache = (spireToApache[Double] _) compose breezeToSpire

  //////////////////////////

  def realToSpire[A: Fractional: Trig](
    self: A): SpireComplex[A] =
    new SpireComplex(self, implicitly[Fractional[A]].zero)

  def realToApache[A <% Double: spire.math.Numeric: Fractional: Trig](
    self: A): ApacheComplex =
    spireToApache(realToSpire(self))

  def realToBreeze[A <% Double: spire.math.Numeric: Fractional: Trig](
    self: A): BreezeComplex =
    spireToBreeze(realToSpire(self))

  def spireToReal[A: spire.math.Numeric](self: SpireComplex[A]): A = {
    requirey(self.imag - implicitly[Numeric[A]].zero <= 0.00001)
    self.real
  }

  def apacheToReal(self: ApacheComplex): Double =
    spireToReal(apacheToSpire(self))

  def breezeToReal(self: BreezeComplex): Double =
    spireToReal(breezeToSpire(self))
}

