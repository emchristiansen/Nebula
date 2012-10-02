package nebula.scratchpad

trait Blah[A] {
  val a: A
}

object Blah {
  type BlahSuper = Blah[_]
}

//trait Descriptor
//
//object Descriptor {
//  implicit def myDescriptor: Descriptor = new Descriptor 
//}
//
//case class MyDescriptor()

trait Detector

trait Extractor[D] {
  def extract: D
}

object Extractor {
  implicit def myExtractor(a: MyExtractor): Extractor[Int] = new Extractor[Int] {
    override def extract = a.extract
  } 
}

case class MyExtractor() {
  def extract: Int = 42
}


trait Matcher[D] {
  def distance(a: D, b: D): Double
}

object Matcher {
  implicit def myMatcher(a: MyMatcher): Matcher[Int] = new Matcher[Int] {
    override def distance(c: Int, b: Int) = a.distance(c, b)
  }
}

case class MyMatcher() {
  def distance(a: Int, b: Int): Double = 1.0
}


case class RuntimeConfig[D](
    val extractor: Extractor[D],
    val matcher: Matcher[D])

object RuntimeConfig {
  def run[D](config: RuntimeConfig[D]): Double = {
    val feature: D = config.extractor.extract
    config.matcher.distance(feature, feature)
  }
}