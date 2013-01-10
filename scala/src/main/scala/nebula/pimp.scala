package nebula

///////////////////////////////////////////////////////////

trait HasOriginal {
//  type Foo[A, B] = A => B
//  
//  type Foo1 = Foo[Int, _]
//  
//  def bar[A: Foo[Int]] {
//    
//  }
  
//  trait ExtractorT[A, B]
//  
//  trait ExtractorT3[C[_]]
//  
//  trait ExtractorT2[A] extends ExtractorT3[({type X[B] = ExtractorT[A, B]})#X] 
//  
//  trait Argh[A[B]]
//  
//  def foo[E: Argh[Int]] {
//    
//  }
//  
//  trait Qux[A, B]
//  
//  trait Turkle[C[_]]
//  
//  trait Baz[A] extends Turkle[({type x[a]=Qux[A, a]})#x]
  
//  type OriginalType 
//  
//  trait Baz[A] extends Turkle[({type x[a]=Qux[A, a]})#x]
  
  def original: Any
  
  override def toString = original.toString
  
  override def equals(obj: Any) = obj match {
    case obj: HasOriginal => original.equals(obj.original)
    case _ => false
  }
  
  override def hashCode = original.hashCode
}