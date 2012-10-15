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

//trait Extractor[D] {
//  def extract: D
//}
//
//object Extractor {
//  implicit def myExtractor(a: MyExtractor): Extractor[Int] = new Extractor[Int] {
//    override def extract = a.extract
//  }
//}
//
//case class MyExtractor() {
//  def extract: Int = 42
//}
//
//trait Matcher[D] {
//  def distance(a: D, b: D): Double
//}
//
//object Matcher {
//  implicit def myMatcher(a: MyMatcher) = new Matcher[Int] {
//    override def distance(c: Int, b: Int) = a.distance(c, b)
//  }
//
//  implicit def myMatcher2(m: MyMatcher2) = new Matcher[String] {
//    override def distance(a: String, b: String) = m.distance(a, b)
//  }
//}
//
//case class MyMatcher() {
//  def distance(a: Int, b: Int): Double = 1.0
//}
//
//case class MyMatcher2() {
//  def distance(a: String, b: String): Double = 2.0
//}
//
//case class RuntimeConfig[D](
//  val extractor: Extractor[D],
//  val matcher: Matcher[D]) {
//  def run: Double = RuntimeConfig.run(this)
//}
//
//object RuntimeConfig {
//  type RuntimeConfigSuper = RuntimeConfig[_]
//
//  def run[D](config: RuntimeConfig[D]): Double = {
//    val feature: D = config.extractor.extract
//    config.matcher.distance(feature, feature)
//  }
//}
//
//object MyApp {
//  val extractor = MyExtractor()
//  val matcher = MyMatcher()
//  val matcher2 = MyMatcher2()
//  val config = RuntimeConfig(extractor, matcher)
//  config.run
//  
//  RuntimeConfig.run(config)
//}

//case class Foo[A](val makeIt: () => A, val eatIt: A => Unit) {
//  def insideBar = Foo.outsideBar(this)
//}
//
//object Foo {
//  def outsideBar[A](foo: Foo[A]) {
//    val it: A = foo.makeIt()
//    foo.eatIt(it)
//  }
//
//  val foo =
//    if ((new util.Random).nextInt > 0) Foo[Int](() => 42, x => println("got int", x))
//    else Foo[String](() => "hi", x => println("got string", x))
//
////  foo.insideBar
//  outsideBar(foo)
//}

trait Bar {
  type MyType
}

object Bar {
  def compareTypes[L <: Bar, R <: Bar](left: L, right: R)(
    implicit ev: L#MyType =:= R#MyType = null
  ): Boolean = ev != null
}

object Foo {
  def foo[A, B <% A](a: A, b: B) {}
  
  foo(1.0, 1)
}

//val intBar1 = new Bar { type MyType = Int }
//val intBar2 = new Bar { type MyType = Int }
//val strBar1 = new Bar { type MyType = String }
//
//Bar.compareTypes(intBar1, intBar2)


//object Bar extends Bar {
//  def compareTypes(left: Bar, right: Bar): Boolean = (left.MyType == right.MyType)
//}

trait Descriptor {
  val values: IndexedSeq[Any]
}

case class SortDescriptor(override val values: IndexedSeq[Int]) extends Descriptor

case class RawDescriptor[E](override val values: IndexedSeq[E]) extends Descriptor

trait Extractor {
  def extract: () => Descriptor
}

case class SortExtractor() extends Extractor {
  def extract = () => SortDescriptor(IndexedSeq[Int]())
}

case class RawExtractor() extends Extractor {
  def extract = () => RawDescriptor(IndexedSeq[Byte]())
}

trait Matcher {
  def doMatch: Descriptor => Int
}

case class L0Matcher() extends Matcher {
  override def doMatch = (d: Descriptor) => 0 
}

case class CayleyMatcher() extends Matcher {
  override def doMatch = d => d match {
    case x: SortDescriptor => 1
  }
}

case class Experiment(val extractor: Extractor, val matcher: Matcher)

object MyApp {
  val experiment = Experiment(RawExtractor(), L0Matcher())
  val descriptor = experiment.extractor.extract()
  val result = experiment.matcher.doMatch(descriptor)
}

trait MyPrint {
  def myPrint: Any
}

trait MyIntPrint extends MyPrint {
  def myIntPrint: Any
}

trait MyStringPrint extends MyPrint {
  def myStringPrint: Any
}

case class D1(value: Int)

object D1 {
  implicit def implicitMyIntPrint(self: D1) = new MyIntPrint {
    def myPrint = myIntPrint
    def myIntPrint = "int" + self.value
  }
}

case class D2(value: String)

object D2 {
  implicit def implicitMyStringPrint(self: D1) = new MyStringPrint {
    def myPrint = myStringPrint
    def myStringPrint = "string" + self.value
  }
}

case class HasPrinter(printer: MyPrint)

object MyApp2 {
  val d1 = D1(10)
  val hasPrinter = HasPrinter(d1)
  hasPrinter.printer.myPrint
}

