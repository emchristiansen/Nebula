import com.twitter.util.Eval
import java.io.File

///////////////////////////////////////////////////////////

package object nebula {
  implicit class IntTimes(int: Int) {
    def times[A](function: => A): IndexedSeq[A] =
      (0 until int).map(_ => function)
  }

  def TODO = sys.error("TODO")

  def eval[A: Manifest](expression: String): A = {
    val source = """
      import nebula._;
      import nebula.smallBaseline._;
      import nebula.wideBaseline._;
      import spark.SparkContext;
      import SparkContext._;
      
      val value: %s = { %s };
      value""".format(implicitly[Manifest[A]], expression)

    (new Eval).apply[A](source)
  }
  
  def evalFile[A: Manifest](file: File): A = {
    val string = org.apache.commons.io.FileUtils.readFileToString(file)
    eval[A](string)
  }

  implicit class AddAssert[A](value: A) {
    def assert(assertions: (A => Boolean)*) = {
      assertions.map(assertion => Predef.assert(assertion(value)))
      value
    }
  }

  def assertNear(threshold: Double, left: Double, right: Double): Unit = {
    Predef.assert(
      (left - right).abs <= threshold,
      "left, right: %s, %s".format(left, right))
  }

  def assertNear2(left: Double, right: Double): Unit =
    assertNear(0.00001, left, right)

  implicit class AddTo[A](obj: A) {
    def to[B](implicit conversion: A => B): B = conversion(obj)
  }
}