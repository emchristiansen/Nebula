package nebula.util

import java.awt.image.BufferedImage

///////////////////////////////////////////////////////////

object GraphicsUtil {
  def drawSideBySide(leftImage: BufferedImage, rightImage: BufferedImage): BufferedImage = {    
    val canvas = new BufferedImage(
        leftImage.getWidth + rightImage.getWidth,
        math.max(leftImage.getHeight, rightImage.getHeight),
        leftImage.getType)
    
    val graphics = canvas.getGraphics
    graphics.drawImage(leftImage, 0, 0, null)
    graphics.drawImage(rightImage, leftImage.getWidth, 0, null)
    canvas
  }
}