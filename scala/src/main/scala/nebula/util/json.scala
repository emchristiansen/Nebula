package nebula.util

import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.Serialization
import net.liftweb.json.Formats
import net.liftweb.json.Serialization
import net.liftweb.json.Serialization.read
import net.liftweb.json.Serialization.write
import net.liftweb.json.ShortTypeHints
import net.liftweb.json.parse
import net.liftweb.json.pretty
import net.liftweb.json.render
import net.liftweb.json.JsonAST.JField
import net.liftweb.json.JsonAST.JString
import scala.text.{ Document, DocText }
import net.liftweb.json.JsonAST.JValue
import spray.json.RootJsonFormat
import spray.json.JsString
import spray.json.JsValue

import spray.json._

///////////////////////////////////////////////////////////

trait EnumerationJsonFormat[A] extends RootJsonFormat[A] {
  override def write(e: A) = JsString(e.toString)

  val expectedType: String
  val deserializeMapping: Map[String, A]

  // TODO: Duplication
  override def read(value: JsValue) = value match {
    case JsString(string) => deserializeMapping.getOrElse(
      string,
      throw new DeserializationException("%s expected".format(expectedType)))
    case _ => throw new DeserializationException("%s expected".format(expectedType))
  }
}

trait JSONSerializable {
  def json: JValue
}

object JSONUtil {
  def toJSON[A <: AnyRef](caseClass: A, extraInstances: List[Class[_]]): JValue = {
    // This should work for non-nested case classes.
    implicit val formats = Serialization.formats(
      ShortTypeHints(caseClass.getClass :: extraInstances))
    parse(write(caseClass))
  }

  def caseClassToStringMap[A <: AnyRef](caseClass: A): Map[String, String] = {
    // Implementation uses lift-json for introspection, which is
    // admittedly roundabout. It is also likely brittle; I'm guessing it will
    // fail for nested structures.
    // TODO: Use introspection directly instead of lift-json.
    implicit val formats = Serialization.formats(ShortTypeHints(List(caseClass.getClass)))

    val string = write(caseClass)
    val json = parse(string)

    def documentToString(document: Document): String = document match {
      case DocText(string) => string.replace("\"", "")
      case _ => throw new Exception
    }

    val JObject(jObject) = json
    val jStrings = jObject.map({ case JField(key, value) => JField(key, JString(documentToString(render(value)))) })
    val jsonString = JObject(jStrings)

    jsonString.extract[Map[String, String]]
  }

  def abbreviate[A <: AnyRef](caseClass: A): String = {
    // For a case class like
    // > case class AwesomeDetector(theFirstParameter: String, second: Int)
    // and 
    // > val ad = AwesomeDetector("helloWorld", 42)
    // produces an abbreviation like
    // "AwesomeDetector-TFP-helloWorld-S-42".
    val map = JSONUtil.caseClassToStringMap(caseClass)

    def camelCaseToAbbreviation(camelCase: String): String = {
      // Assume camelCase for parameter names. Otherwise
      // the abbreviations will be weird.
      // Example: "myCoolValue" becomes "MCV".
      camelCase.head.toUpper + camelCase.filter(_.isUpper)
    }

    val parameters = map.filterKeys(_ != "jsonClass").toList.sortBy(_._1)
    val parameterNames = parameters.map(p => camelCaseToAbbreviation(p._1))
    val parameterValues = parameters.map(p => p._2)

    val parameterParts = List(parameterNames, parameterValues).transpose.flatten
    val parts = map("jsonClass") :: parameterParts
    parts.mkString("-")
  }
}