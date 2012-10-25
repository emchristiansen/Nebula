package nebula

///////////////////////////////////////////////////////////

trait HasOriginal {
  def original: Any
  override def toString = original.toString
}