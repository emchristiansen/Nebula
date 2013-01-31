import scala.reflect.runtime._
import scala.reflect.runtime
import scala.reflect._
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.universe.typeTag
import scala.util._
import spray.json._
import nebula.wideBaseline.WideBaselineJsonProtocol
import shapeless._
import java.io.File
import nebula.util._
import nebula.brown._

///////////////////////////////////////////////////////////

package object nebula {
  lazy val loadOpenCV = System.loadLibrary("opencv_java")
  
  // TODO: Uncomment this function and change the relevant constructors.
  // Currently this is impossible due to a probable Scala bug.
//  implicit def experimentRunnerInsertRuntime[A <% RuntimeConfig => ExperimentRunner[B], B](
//    a: A)(
//      implicit runtimeConfig: RuntimeConfig): ExperimentRunner[B] =
//    implicitly[A => RuntimeConfig => ExperimentRunner[B]].apply(a).apply(runtimeConfig)

  val homeDirectory = new File(System.getProperty("user.home"))

  def getResource(path: String): File =
    new File(getClass.getResource(path).getFile).mustExist

  implicit class PimpFile(file: File) {
    def mustExist: File = {
      assert(file.exists, s"File ${file} doesn't exist")
      file
    }

    def parentMustExist: File = {
      new File(file.getParent).mustExist
      file
    }

    def +(that: String): File = new File(file, that)
  }

  object JsonProtocols extends 
  DetectorJsonProtocol with 
  ExtractorJsonProtocol with 
  PatchNormalizerJsonProtocol with
  MatcherJsonProtocol with
  WideBaselineJsonProtocol with 
  BrownJsonProtocol with
  DMatchJsonProtocol

  implicit class IntTimes(int: Int) {
    def times[A](function: => A): IndexedSeq[A] =
      (0 until int).map(_ => function)
  }

  case class Epsilon(value: Double)

  object Epsilon {
    implicit def toDouble(self: Epsilon): Double = self.value
  }

  implicit val epsilon = Epsilon(0.0001)

  case class Imports(packages: Set[String])

  object Imports {
    implicit def toSetString(self: Imports): Set[String] = self.packages
  }

  val nebulaImports = Imports(Set(
    "nebula._",
    "nebula.smallBaseline._",
    "nebula.wideBaseline._",
    "nebula.summary._",
    "nebula.Distributed._"))

  val jsonImports = Imports(Set(
    "spray.json._",
    "nebula.JsonProtocols._"))

  val sparkImports = Imports(Set(
    "spark.SparkContext",
    "spark.SparkContext._"))

  val shapelessImports = Imports(Set(
    "shapeless._"))
    
  val reflectImports = Imports(Set(
    "reflect.runtime.universe._"))

  implicit val allImports = Imports(nebulaImports ++ jsonImports ++ sparkImports ++ shapelessImports ++ reflectImports)

  // TODO: Replace with ???
  def TODO = sys.error("TODO")

  implicit class AddImportsToSource(source: String) {
    def addImports(implicit imports: Imports): String = {
      val formattedImports = imports map { x => s"import ${x}" }
      val importString = formattedImports.toList.sorted.mkString("\n")
      importString + "\n\n" + source
    }
  }

  case class TypeName[A: TypeTag](name: String) {
    assert(typeTag[A].tpe.toString == name)
    override def toString = name
  }

  object TypeName {
    implicit def typeName2String(typeName: TypeName[_]): String = typeName.name
  }

  def typeName[A: TypeName] = implicitly[TypeName[A]]

  implicit def typeTag2TypeName[A: TypeTag]: TypeName[A] = TypeName(typeTag[A].tpe.toString)

  // The compiler crashes if you invoke it several times simultaneously.
  val compilerLock: AnyRef = new Object()

  /**
   * Checks the type of the given expression.
   * If the type check passes, returns a closure that can be run to evaluate
   * the expression.
   */
  def typeCheck[A: TypeName](expression: String): () => A = {
    val source = s"""
val result: ${typeName[A]} = {${expression}}
result
    """

    println(source)

    // From http://stackoverflow.com/questions/12122939/generating-a-class-from-string-and-instantiating-it-in-scala-2-10/12123609#12123609
    val cm = universe.runtimeMirror(getClass.getClassLoader)
    val toolbox = cm.mkToolBox()

    toolbox.compile(toolbox.parse(source)).asInstanceOf[() => A]
  }

  //  def typeCheck[A: TypeTag](expression: String): () => A = 
  //    typeCheck(typeTag[A].tpe.toString, expression)

  def eval[A: TypeName](expression: String): A = {
    typeCheck[A](expression).apply()
  }

  //  def eval[A: TypeTag](expression: String): A = 
  //    eval(typeTag[A].tpe.toString, expression)

  def implicitlyExpression[A: TypeName]: String = s"implicitly[${typeName[A]}]"

  //  def implicitlyExpression[A: TypeTag]: String = implicitlyExpression[A](typeTag[A].tpe.toString)

  // TODO: See if I can instead introduce
  // type TypeName[A] = String
  // to get serializable type names implicitly.
  def runtimeImplicitly[A: TypeName]: A =
    eval[A](implicitlyExpression[A](typeName).addImports)

  //  def runtimeImplicitly[A: TypeTag](): A = 
  //    runtimeImplicitly[A](typeTag[A].tpe.toString)

  //  // These are useful enough to import by default.
  //  type FileUtils = org.apache.commons.io.FileUtils
  //  type File = java.io.File 

  def generateExpression[A: JsonFormat: TypeName](a: A): String =
    s"""
val jsonString: String = \"\"\"${a.toJson.compactPrint}\"\"\"
jsonString.asJson.convertTo[${typeName[A]}]
"""

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

  /**
   * Checks the ratio of two positive numbers is close to 1.
   */
  def assertRelativelyNear(maxRatio: Double)(left: Double, right: Double) {
    require(maxRatio >= 1)
    require(left > 0)
    require(right > 0)

    assert(left / right <= maxRatio, s"${left} / ${right} = ${left / right} > ${maxRatio}")
    assert(right / left <= maxRatio, s"${right} / ${left} = ${right / left} > ${maxRatio}")
  }

  implicit class AddTo[A](obj: A) {
    def to[B](implicit conversion: A => B): B = conversion(obj)
  }

  implicit class AddCompose[-T1, +R](function1: (T1) => R) {
    def compose[A, B](function2: (A, B) => T1): (A, B) => R = (a, b) => function1(function2(a, b))
  }

}