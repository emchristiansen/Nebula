import com.twitter.util.Eval

///////////////////////////////////////////////////////////

package object nebula {
  //  // TODO: Test if these imports do anything.
  //  import mpie._
  //  import smallBaseline._
  //  import summary._
  //  import util._
  //  import util.imageProcessing._
  //  import wideBaseline._

  implicit def intTimes(int: Int) = new {
    def times[A](function: => A): IndexedSeq[A] =
      (0 until int).map(_ => function)
  }

  def TODO = sys.error("TODO")

  def eval[A: Manifest](expression: String) = {
    val source = """
      import nebula._;
      import nebula.smallBaseline._;
      import nebula.wideBaseline._;
      
      val value: %s = { %s };
      value""".format(implicitly[Manifest[A]], expression)

    (new Eval).apply[A](source)
  }

  implicit def addAssert[A](value: A) = new {
    def assert(assertions: (A => Boolean)*) = {
      assertions.map(assertion => Predef.assert(assertion(value)))
      value
    }
  }

  def assertNear(threshold: Double, left: Double, right: Double) {
    Predef.assert(
      (left - right).abs <= threshold,
      "left, right: %s, %s".format(left, right))
  }
}