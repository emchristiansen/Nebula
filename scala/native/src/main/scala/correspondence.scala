package nebula

import java.awt.image._
import java.io.File
import xml._

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}

import com.googlecode.javacv.cpp.opencv_contrib._
import com.googlecode.javacv.cpp.opencv_core._
import com.googlecode.javacv.cpp.opencv_features2d._


trait CorrespondenceMethod {
  def abbreviation: String = {
    // For a case class like
    // > case class AwesomeDetector(theFirstParameter: String, second: Int)
    // and 
    // > val ad = AwesomeDetector("helloWorld", 42)
    // produces an abbreviation like
    // "AwesomeDetector-TFP-helloWorld-S-42".
    // Implementation uses lift-json for introspection, which is
    // admittedly roundabout.

    // This is needed to include class information in the json.
    implicit val formats = Serialization.formats(ShortTypeHints(List(this.getClass())))

    val string = write(this)
    val json = parse(string)
    val map = json.extract[Map[String, String]]

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


