package nebula
import scala.reflect.runtime._
import scala.reflect._
import scala.tools.reflect.ToolBox
import org.apache.commons.io.FileUtils
import scala.util.Try
import spray.json._
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.universe.typeTag

///////////////////////////////////////////////////////////

/**
 * Container for blocks of import statements in Scala source code.
 */
case class Imports(packages: Set[String])

object Imports {
  def apply(packages: String*): Imports = Imports(packages.toSet)

  implicit def toSetString(self: Imports): Set[String] = self.packages
}

/**
 * Represents the name of a type as a string.
 */
case class TypeName[A: TypeTag](name: String) {
  assert(typeTag[A].tpe.toString == name)
  override def toString = name
}

object TypeName {
  implicit def typeName2String(typeName: TypeName[_]): String = typeName.name
}

/**
 * Tools for evaluating Scala source code at runtime.
 */
trait Eval {
  /**
   * Pimp to add a set of imports to the top of a source code string.
   */
  implicit class AddImportsToSource(source: String) {
    def addImports(implicit imports: Imports): String = {
      val formattedImports = imports map { x => s"import ${x}" }
      val importString = formattedImports.toList.sorted.mkString("\n")
      importString + "\n\n" + source
    }
  }

  /**
   * Pimp to include extra source at the top of a file.
   * Similar to C-style #include
   */
  implicit class AddIncludeToSource(source: String) {
    def include(header: String): String =
      s"${header}\n\n// ABOVE CODE AUTOMATICALLY INCLUDED\n\n${source}"

    def include(files: Seq[ExistingFile]): String = {
      val headers = files map (_.file) map FileUtils.readFileToString
      include(headers mkString "\n\n//FILE DIVIDER\n\n")
    }
  }

  ///////////////////////////////////////////////////////////

  /**
   * Convenience function to get the name of a type.
   */
  def typeName[A: TypeName] = implicitly[TypeName[A]]

  implicit def typeTag2TypeName[A: TypeTag]: TypeName[A] =
    TypeName(typeTag[A].tpe.toString)

  ///////////////////////////////////////////////////////////

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

    // From http://stackoverflow.com/questions/12122939/generating-a-class-from-string-and-instantiating-it-in-scala-2-10/12123609#12123609
    val cm = universe.runtimeMirror(getClass.getClassLoader)
    val toolbox = cm.mkToolBox()

    try {
      toolbox.compile(toolbox.parse(source)).asInstanceOf[() => A]
    } catch {
      case e: Any => {
        val sourceHeader = "// BEGIN: THIS SOURCE FAILED TO COMPILE"
        val sourceFooter = "// END: THIS SOURCE FAILED TO COMPILE"
        println(Seq(
          sourceHeader,
          source,
          sourceFooter) mkString ("\n"))
        throw e
      }
    }
  }

  def hasType[A: TypeName](expression: String): Boolean =
    Try(typeCheck[A](expression)).isSuccess

  /**
   * Type checks and evaluates a given expression.
   */
  def eval[A: TypeName](expression: String): A = {
    typeCheck[A](expression).apply()
  }

  def implicitlyType[A: TypeName]: String = s"implicitly[${typeName[A]}]"

  /**
   * Finds an instance of the desired type at runtime.
   */
  def runtimeImplicitly[A: TypeName](implicit imports: Imports): A =
    eval[A](implicitlyType[A](typeName).addImports)

  /**
   * Transforms a value into source that yields that value, for types
   * supporting JsonFormat.
   */
  def generateExpression[A: JsonFormat: TypeName](a: A): String =
    s"""
val jsonString: String = \"\"\"${a.toJson.compactPrint}\"\"\"
jsonString.asJson.convertTo[${typeName[A]}]
"""
}