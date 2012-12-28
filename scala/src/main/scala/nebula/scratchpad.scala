package nebula.scratchpad

// Code in this file is just used for playing around, you shouldn't build against it.

trait FooTrait {
  def foo: String
  def bar: Int
}

trait Extractor[A] {
  def extract: () => A
}

object ExtractorInt extends Extractor[Int] {
  override def extract = () => 42
} 

object ExtractorString extends Extractor[String] {
  override def extract = () => "hello"
}

trait Matcher[A] {
  def doMatch: (A, A) => Unit
}

object MatcherInt extends Matcher[Int] {
  override def doMatch = (a, b) => ()
}

//object MatcherString extends Matcher[String] {
//  override def doMatch
//}

case class Experiment[A](extractor: Extractor[A], matcher: Matcher[A])

//object Foo {
//  implicit object Foo[A] {
//    
//  }
//}



