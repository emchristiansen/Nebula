package nebula

/////////////////////////////////////

/**
 * A container with one element.
 */
// TODO: Is this just an Option?
trait Box[A] {
  def get: A
}