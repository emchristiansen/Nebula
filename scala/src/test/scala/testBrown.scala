import nebula._
import org.scalatest.FunSuite
import javax.imageio.ImageIO
import java.io.File

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import nebula.util._

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
class TestBrown extends FunSuite {
  ignore("loadPatchPairs should return reasonable pairs") {
    val datasetName = "liberty"
    val numMatches = 1000
    val dataRoot = new File(homeDirectory, "Bitcasa/data").mustExist
    
    val patchPairs = PatchPair.loadPatchPairs(datasetName, numMatches, dataRoot)
    
    for ((patchPair, index) <- patchPairs.zipWithIndex) {
      val sideBySide = GraphicsUtil.drawSideBySide(patchPair.left.image, patchPair.right.image)
      sideBySide.getGraphics.drawString(patchPair.corresponds.toString, 5, 20)
      
      TestUtil.dumpImage(s"${index}", sideBySide)
    }
  }
}