package nebula

///////////////////////////////////////////////////

/**
 * A quantity measured in radians.
 */
case class Radians(radians: Double)

object Radians {
  implicit def radiansToDouble(radians: Radians) = radians.radians
}