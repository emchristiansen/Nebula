package nebula.scratchpad

// Code in this file is just used for playing around, you shouldn't build against it.

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
  implicit def myMatcher(a: MyMatcher) = new Matcher[Int] {
    override def distance(c: Int, b: Int) = a.distance(c, b)
  }

  implicit def myMatcher2(m: MyMatcher2) = new Matcher[String] {
    override def distance(a: String, b: String) = m.distance(a, b)
  }
}

case class MyMatcher() {
  def distance(a: Int, b: Int): Double = 1.0
}

case class MyMatcher2() {
  def distance(a: String, b: String): Double = 2.0
}

case class RuntimeConfig[D](
  val extractor: Extractor[D],
  val matcher: Matcher[D]) {
  def run: Double = RuntimeConfig.run(this)
}

object RuntimeConfig {
  type RuntimeConfigSuper = RuntimeConfig[_]

  def run[D](config: RuntimeConfig[D]): Double = {
    val feature: D = config.extractor.extract
    config.matcher.distance(feature, feature)
  }
}

object MyApp {
  val extractor = MyExtractor()
  val matcher = MyMatcher()
  val matcher2 = MyMatcher2()
  val config = RuntimeConfig(extractor, matcher)
  config.run
  
  RuntimeConfig.run(config)
}

case class Foo[A](val makeIt: () => A, val eatIt: A => Unit) {
  def insideBar = Foo.outsideBar(this)
}

object Foo {
  def outsideBar[A](foo: Foo[A]) {
    val it: A = foo.makeIt()
    foo.eatIt(it)
  }

  val foo =
    if ((new util.Random).nextInt > 0) Foo[Int](() => 42, x => println("got int", x))
    else Foo[String](() => "hi", x => println("got string", x))

  foo.insideBar
  outsideBar(foo)
}
