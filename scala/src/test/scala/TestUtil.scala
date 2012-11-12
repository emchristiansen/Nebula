import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.io.File

import nebula.util.DenseMatrixUtil._
import nebula._

object TestUtil {
  def dumpImage(name: String, image: BufferedImage) {
    val tempDirectory = File.createTempFile("temp", "").getParentFile
    assert(tempDirectory != null)
    val testDirectory = new File(tempDirectory, "nebulaTest")
    println(testDirectory)
    if (!testDirectory.isDirectory) testDirectory.mkdir

    val file = new File(testDirectory, "%s.png".format(name))
    println("writing %s".format(file))
    println("image is size %s by %s".format(image.getWidth, image.getHeight))
    assert(ImageIO.write(image, "png", file))
  }
  
  def scaleUp(scaleFactor: Int)(image: BufferedImage): BufferedImage = {
    require(scaleFactor >= 1)
    
    val seqSeq = image.toMatrix.toSeqSeq
    val wider = seqSeq.map(_.flatMap(x => scaleFactor times x))
    val taller = wider.flatMap(row => scaleFactor times row)
    taller.toMatrix.toImage
  }
  
  def scale10 = scaleUp(10) _
  def scale100 = scaleUp(100) _
}