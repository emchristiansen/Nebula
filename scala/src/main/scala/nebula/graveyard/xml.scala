//package nebula.graveyard
//
//import nebula._
//
//import java.io.File
//
//import scala.xml.{Node, XML}
//
//import nebula.util.IO
//
/////////////////////////////////////////////////////////////
//
//trait FromXML[+A] {
//  def fromXMLOption(node: Node): Option[A]
//  def fromXML(node: Node): A = fromXMLOption(node).get
//  def fromFile(file: File): A = fromXML(XML.loadFile(file))
//}
//
//object XMLUtil {
//  def mapOnKey[A](function: (Node) => A, key: String)(implicit node: Node): List[A] =
//    (node \ key).toList.map(function)
//
//  def singleOnKey[A](function: (Node) => A, key: String)(implicit node: Node): A = {
//    val list = mapOnKey(function, key)
//    asserty(list.size == 1)
//    list.head
//  }
//
//  def text(node: Node)(key: String): String = {
//    val list = (node \ key).toList
//    asserty(list.size == 1)
//    list.head.text
//  }
//
//  def save(path: String, node: Node)(implicit runtime: RuntimeConfig) {
//    val temp = IO.createTempFile("unformattedXML", ".xml")
//    XML.save(temp.toString, node)
//    val command = "xmllint --format %s --output %s".format(temp.toString, path)
//    IO.runSystemCommand(command)
//  }
//}