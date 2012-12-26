package nebula.util

import nebula._

//import net.liftweb.json.JsonAST.JObject
//import net.liftweb.json.Serialization
//import net.liftweb.json.Formats
//import net.liftweb.json.Serialization
//import net.liftweb.json.Serialization.read
//import net.liftweb.json.Serialization.write
//import net.liftweb.json.ShortTypeHints
//import net.liftweb.json.parse
//import net.liftweb.json.pretty
//import net.liftweb.json.render
//import net.liftweb.json.JsonAST.JField
//import net.liftweb.json.JsonAST.JString
import scala.text.{ Document, DocText }
//import net.liftweb.json.JsonAST.JValue
import spray.json.RootJsonFormat
import spray.json.JsString
import spray.json.JsValue

import spray.json._
import scala.util.matching.Regex

///////////////////////////////////////////////////////////

//trait JSONSerializable {
//  def json: JValue
//}

object JSONUtil extends Logging {
  def enumeration[A](scalaClass: String, deserializeMapping: Map[String, A]): RootJsonFormat[A] =
    new RootJsonFormat[A] {
      override def write(e: A) = JsString(e.toString)

      // TODO: Duplication
      override def read(value: JsValue) = {
        value match {
          case JsString(string) => deserializeMapping.get(string) match {
            case Some(x) => x
            case None => throw new DeserializationException("%s expected".format(scalaClass))
          }
          case _ => throw new DeserializationException("%s expected".format(scalaClass))
        }
      }
    }

  //      new RootJsonFormat[A] {
  //      override def write(e: A) = JsObject(
  //        "matcherType" -> JsString(e.toString),
  //        "scalaClass" -> JsString(scalaClass))
  //
  //      // TODO: Duplication
  //      override def read(value: JsValue) = {
  //        if (value.asJsObject.fields("scalaClass") != scalaClass)
  //          throw new DeserializationException("%s expected".format(scalaClass))
  //        else {
  //          value.asJsObject.fields("matcherType") match {
  //            case JsString(string) => deserializeMapping.getOrElse(
  //              string,
  //              throw new DeserializationException("%s expected".format(scalaClass)))
  //            case _ => throw new DeserializationException("%s expected".format(scalaClass))
  //          }
  //        }
  //      }
  //    }
  
 
  implicit class AddClassName[A](self: RootJsonFormat[A]) {
    def addClassInfo(scalaClass: String): RootJsonFormat[A] = new RootJsonFormat[A] {
      override def write(e: A) = {
        val fields = self.write(e).asJsObject.fields
        assert(!fields.contains("scalaClass"))
        JsObject(fields + ("scalaClass" -> JsString(scalaClass)))
      }
      override def read(value: JsValue) = {
        val fields = value.asJsObject.fields
        assert(fields.contains("scalaClass"))
        self.read(JsObject(fields - "scalaClass"))
      }
    }
  }

  def camelCaseToAbbreviation(camelCase: String): String = {
    // Assume camelCase for parameter names. Otherwise
    // the abbreviations will be weird.
    // Example: "myCoolValue" becomes "MCV".
    camelCase.head.toUpper + camelCase.filter(_.isUpper)
  }

  def sortJson(json: JsValue)(
    implicit ordering: Ordering[String]): JsValue = json match {
    case JsObject(json) => {
      val sortedValues = json.mapValues(sortJson)
      JsObject(Util.sortMap(sortedValues))
    }
    case x => x
  }

  def jsonToFileString(json: JsValue): String = {
    val jsonString = json.compactPrint

    // Now we clean it up.
    // Remove quotation marks.
    val one = jsonString.replace("\"", "")

    // Remove "scalaClass".
    val two = one.replace("scalaClass:", "")

    // Abbreviate parameter names.    
    val regex = new Regex("""([A-Za-z]+):""", "parameter")
    val three = regex replaceAllIn (
      two,
      m => camelCaseToAbbreviation(m.group("parameter")) + ":")

    // Remove beginning and ending crap.
    val four = three.dropWhile(_ == '{').reverse dropWhile (_ == '}') reverse

    // Replace some characters.
    four.replace(":", "-").replace(",", "_")
  }

  def scalaClassSortJson(json: JsValue): JsValue = {
    // Make an ordering that brings "scalaClass" to front.
    val ordering: Ordering[String] = new Ordering[String] {
      override def compare(left: String, right: String) = {
        if (left == "scalaClass") -1
        else if (right == "scalaClass") 1
        else implicitly[Ordering[String]].compare(left, right)
      }
    }

    // Get the JSON string with the fields arranged the way we want
    // them.    
    sortJson(json)(ordering)
  }

  // Flattens a JSON tree.
  def flattenJson(json: JsValue): String = {
    def sorted = scalaClassSortJson(json)
    jsonToFileString(sorted)
  }

  def getParametersFromJson(json: JsValue): Map[String, String] = json match {
    case JsObject(fields) => {
      fields filterKeys (_ != "scalaClass") map {
        case (key, value) => (camelCaseToAbbreviation(key), flattenJson(value))
      }
    }
    case _ => sys.error("Must pass a JsObject")
  }

  //  // Flattens a JSON tree. Assumes each map has a "scalaClass" field.
  //  def flattenJson(json: JsValue): String = {
  //    // A temporary value to separate terms in the string.
  //    // These separators are replaced with prettier separators in the
  //    // final string.
  //    def separator(depth: Int): String = "__flattenJSON_%s__".format(depth)
  //
  //    def recurse(json: JsValue, depth: Int): String = json match {
  //      case JsString(string) => string
  //      case JsBoolean(boolean) => boolean.toString
  //      case JsNumber(number) => number.toString
  //      case _ => {
  //        val fields = json.asJsObject.fields
  //        require(fields.keys.toList contains "scalaClass")
  //        
  //        val stringMap = fields filterKeys (_ != "scalaClass") map {
  //          case (key, value) => camelCaseToAbbreviation(key) -> recurse(value, depth + 1)
  //        }
  //
  //        val parameterList = stringMap map {
  //          case (key, value) => "%s-%s".format(key, value)
  //        } toList 
  //        
  //        val parameterString = parameterList.sorted.mkString("", separator(depth), "")
  //        
  //        val className = fields("scalaClass") match {
  //          case JsString(string) => string
  //          case _ => sys.error("scalaClass should be a string")
  //        }
  //        
  //        "%s-%s".format(className, parameterString)
  //      }
  //    }
  //
  //    val withSeparators = recurse(json, 0)
  //    
  //    // The list of depths in the JSON tree.    
  //    val depths = Stream from 0 takeWhile {
  //      depth => withSeparators contains separator(depth)
  //    }
  //
  //    val maxDepth = depths size
  //
  //    // Functions to update |withSeparators|. 
  //    val updates = depths map {
  //      depth =>
  //        (string: String) => string.replace(
  //          separator(depth),
  //          "_" * (maxDepth - depth))
  //    }
  //
  //    // Compose all the update functions.
  //    val composedUpdate = updates.foldLeft(identity _: String => String)(_ compose _)
  //    
  //    composedUpdate(withSeparators)
  //  }

  //  // Drop the root class tag from a flattened JSON tree.
  //  def flattenJsonNoRootClass = (json: JsValue) => flattenJson(json).dropWhile(_ != "-").tail

  //  def toJSON[A <: AnyRef](caseClass: A, extraInstances: List[Class[_]]): JValue = {
  //    // This should work for non-nested case classes.
  //    implicit val formats = Serialization.formats(
  //      ShortTypeHints(caseClass.getClass :: extraInstances))
  //    parse(write(caseClass))
  //  }

  def caseClassToStringMap[A: RootJsonFormat](caseClass: A): Map[String, String] = {
    val json = caseClass.toJson
    val fields = json.asJsObject.fields
    fields.mapValues {
      case JsString(string) => string
      case other => other.toString
    }
  }

  //  def abbreviate[A: RootJsonFormat](caseClass: A): String = {
  //    caseClass.toJson match {
  //      case JsString(string) => string
  //      case _ => {
  //        val map = JSONUtil.caseClassToStringMap(caseClass)
  //
  //        val parameters = map.filterKeys(_ != "scalaClass").toList.sortBy(_._1)
  //        val parameterNames = parameters.map(p => camelCaseToAbbreviation(p._1))
  //        val parameterValues = parameters.map(p => p._2)
  //
  //        val parameterParts = List(parameterNames, parameterValues).transpose.flatten
  //        val parts = map("scalaClass") :: parameterParts
  //        parts.mkString("-")
  //      }
  //    }
  //  }

  //  def caseClassToStringMap[A <: AnyRef](caseClass: A): Map[String, String] = {
  //    // Implementation uses lift-json for introspection, which is
  //    // admittedly roundabout. It is also likely brittle; I'm guessing it will
  //    // fail for nested structures.
  //    // TODO: Use introspection directly instead of lift-json.
  //    implicit val formats = Serialization.formats(ShortTypeHints(List(caseClass.getClass)))
  //
  //    val string = write(caseClass)
  //    val json = parse(string)
  //
  //    def documentToString(document: Document): String = document match {
  //      case DocText(string) => string.replace("\"", "")
  //      case _ => throw new Exception
  //    }
  //
  //    val JObject(jObject) = json
  //    val jStrings = jObject.map({ case JField(key, value) => JField(key, JString(documentToString(render(value)))) })
  //    val jsonString = JObject(jStrings)
  //
  //    jsonString.extract[Map[String, String]]
  //  }
  //
  //  def abbreviate[A <: AnyRef](caseClass: A): String = {
  //    // For a case class like
  //    // > case class AwesomeDetector(theFirstParameter: String, second: Int)
  //    // and 
  //    // > val ad = AwesomeDetector("helloWorld", 42)
  //    // produces an abbreviation like
  //    // "AwesomeDetector-TFP-helloWorld-S-42".
  //    val map = JSONUtil.caseClassToStringMap(caseClass)
  //
  //    def camelCaseToAbbreviation(camelCase: String): String = {
  //      // Assume camelCase for parameter names. Otherwise
  //      // the abbreviations will be weird.
  //      // Example: "myCoolValue" becomes "MCV".
  //      camelCase.head.toUpper + camelCase.filter(_.isUpper)
  //    }
  //
  //    val parameters = map.filterKeys(_ != "jsonClass").toList.sortBy(_._1)
  //    val parameterNames = parameters.map(p => camelCaseToAbbreviation(p._1))
  //    val parameterValues = parameters.map(p => p._2)
  //
  //    val parameterParts = List(parameterNames, parameterValues).transpose.flatten
  //    val parts = map("jsonClass") :: parameterParts
  //    parts.mkString("-")
  //  }
}