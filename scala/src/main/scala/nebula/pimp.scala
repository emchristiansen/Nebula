package nebula

///////////////////////////////////////////////////////////

trait HasOriginal {
  def original: Any
  
  override def toString = original.toString
  
  override def equals(obj: Any) = obj match {
    case obj: HasOriginal => original.equals(obj.original)
    case _ => false
  }
  
  override def hashCode = original.hashCode
}