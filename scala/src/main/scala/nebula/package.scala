import java.io.File
import scala.reflect.runtime._
import scala.reflect.runtime
import scala.tools.reflect.ToolBox

///////////////////////////////////////////////////////////

package object nebula {
  implicit class IntTimes(int: Int) {
    def times[A](function: => A): IndexedSeq[A] =
      (0 until int).map(_ => function)
  }

  def TODO = sys.error("TODO")

  // From http://stackoverflow.com/questions/12122939/generating-a-class-from-string-and-instantiating-it-in-scala-2-10/12123609#12123609
  val cm = universe.runtimeMirror(getClass.getClassLoader)
  val toolbox = cm.mkToolBox()
  
  def eval[A: Manifest](expression: String): A = {
    val source = """
      import nebula._;
      import nebula.smallBaseline._;
      import nebula.wideBaseline._;
      import spark.SparkContext;
      import SparkContext._;
      
      val value: %s = { %s };
      value""".format(implicitly[Manifest[A]], expression)

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