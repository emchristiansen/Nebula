import nebula._
import org.scalatest.FunSuite
import javax.imageio.ImageIO
import java.io.File

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import nebula.util._
import nebula.util.ImageGeometry._
import nebula.imageProcessing._

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
class TestGeometry extends FunSuite {
  val image = ImageIO.read(new File(getClass.getResource("/iSpy.png").getFile).mustExist)
  
  val center = KeyPointUtil(image.getWidth.toFloat / 2, image.getHeight.toFloat / 2)
  
  ignore("rotateAboutPoint") {
    for (theta <- 0.0 until 2 * math.Pi by math.Pi / 16) {
      val rotated = image.rotateAboutPoint(theta, center)
      val patch = ImageUtil.getSubimageCenteredAtPoint(
          rotated,
          center.pt.x,
          center.pt.y,
          20,
          20)
      TestUtil.dumpImage(f"rotated_${theta}%.2f.png", TestUtil.scale10(patch))
    }
  }
  
  ignore("scaleAboutPoint") {
    val exponents = 0.2 until 3.0 by 0.05
    val scaleFactors = exponents.map(e => math.pow(2, e))
    for (scaleFactor <- scaleFactors) {
      val scaled = image.scaleAboutPoint(scaleFactor, center)
      val patch = ImageUtil.getSubimageCenteredAtPoint(
          scaled,
          center.pt.x,
          center.pt.y,
          20,
          20)
      TestUtil.dumpImage(f"scaled_${scaleFactor}%.2f.png", TestUtil.scale10(patch))      
    }
  }
}