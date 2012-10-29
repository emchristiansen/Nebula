import org.apache.commons.math3.linear.{ Array2DRowRealMatrix, ArrayRealVector }
import org.scalatest.FunSuite
import nebula.KeyPointUtil._
import nebula.{ Homography, KeyPointUtil }
import nebula.MatcherParameterized._
import nebula._
import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck._
import math._
import java.io.File
import breeze.linalg._
import scala.util._
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

class TestSmallConfig extends FunSuite {
  ignore("for a very simple image pair the small baseline matchers should be perfect") {
    // The easy image is all black with random pixels in the center.
    // It is simply translated a few pixels to get the other image.    
    val yTranslation = -2
    val xTranslation = 1

    val leftImage = {
      val url = getClass.getResource("/blackSurroundingRandom.bmp")
      ImageIO.read(new File(url.getFile))
    }

    val rightImage = {
      val rightImage = new BufferedImage(leftImage.getWidth, leftImage.getHeight, leftImage.getType)
      for (
        sourceY <- 0 until leftImage.getHeight;
        sourceX <- 0 until leftImage.getWidth
      ) {
        val destY = sourceY + yTranslation
        val destX = sourceX + xTranslation
        try {
          rightImage.setRGB(destX, destY, leftImage.getRGB(sourceX, sourceY))
        } catch {
          case _ => Unit
        }
      }
      rightImage
    }

    val estimatedFlow = SmallBaselineExperiment.estimateFlow(
      2,
      RawExtractor(
        false,
        false,
        16,
        1,
        "Gray"),
      L2Matcher(),
      leftImage,
      rightImage)
    
//    for (((i, j), Some(flowVector)) <- estimatedFlow.iterator)
//      if (flowVector != FlowVector(-2, -2) && flowVector != FlowVector(1, -2)) {
//        println("Bad flow vector", (i, j), flowVector)
//      }
  }
}