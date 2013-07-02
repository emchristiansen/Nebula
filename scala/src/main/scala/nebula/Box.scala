package nebula

/////////////////////////////////////

/**
 * A container with one element, with an implicit conversion to the 
 * wrapped type.
 */
trait Box[A] {
  def get: A
}

object Box {
  implicit def boxToInner[A](box: Box[A]): A = box.get
}
