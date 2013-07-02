package nebula

///////////////////////////////////////////////////

/**
 * A quantity measured in radians.
 */
case class Radians(radians: Double) extends Box[Double] {
  override def get = radians
}