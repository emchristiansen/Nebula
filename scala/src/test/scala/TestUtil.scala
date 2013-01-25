import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.io.File

import nebula.util.imageProcessing._
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
  
  def scale10 = (image: BufferedImage) => ImageUtil.scale(10, image)._2
  def scale100 = (image: BufferedImage) => ImageUtil.scale(100, image)._2
}