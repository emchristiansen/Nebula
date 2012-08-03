import xml._

object XMLUtil {
  def text(node: Node)(key: String): String = {
    val list = (node \ key).toList
    assert(list.size == 1)
    list.head.text
  }

  def save(path: String, node: Node) {
    val temp = Util.createTempFile("unformattedXML", ".xml")
    XML.save(temp.toString, node)
    val command = "xmllint --format %s --output %s".format(temp.toString, path)
    Util.runSystemCommand(command)
  }
}
