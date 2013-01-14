import java.io.File
import scala.reflect.runtime._
import scala.reflect.runtime
import scala.reflect._
import scala.tools.reflect.ToolBox

///////////////////////////////////////////////////////////

package object nebula {
  implicit class IntTimes(int: Int) {
    def times[A](function: => A): IndexedSeq[A] =
      (0 until int).map(_ => function)
  }

  case class Epsilon(value: Double)

  object Epsilon {
    implicit def toDouble(self: Epsilon): Double = self.value
  }

  implicit val epsilon = Epsilon(0.0001)

  // TODO: Replace with ???
  def TODO = sys.error("TODO")

  // From http://stackoverflow.com/questions/12122939/generating-a-class-from-string-and-instantiating-it-in-scala-2-10/12123609#12123609
  val cm = universe.runtimeMirror(getClass.getClassLoader)
  val toolbox = cm.mkToolBox()

  def eval[A: ClassTag](expression: String): A = {
    val classString = classTag[A]

    val source = """
      import nebula._;
      import nebula.smallBaseline._;
      import nebula.wideBaseline._;
      import spark.SparkContext;
      import SparkContext._;
      
      val value: %s = { %s };
      value""".format(classTag, expression)

    toolbox.eval(toolbox.parse(source)).asInstanceOf[A]
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

  def assertNear(
    left: => Double,
    right: => Double)(implicit threshold: Epsilon): Unit = {
    Predef.assert(
      (left - right).abs <= threshold,
      "left, right: %s, %s".format(left, right))
  }

  implicit class AddTo[A](obj: A) {
    def to[B](implicit conversion: A => B): B = conversion(obj)
  }

  implicit class AddCompose[-T1, +R](function1: (T1) => R) {
    def compose[A, B](function2: (A, B) => T1): (A, B) => R = (a, b) => function1(function2(a, b))
  }
}