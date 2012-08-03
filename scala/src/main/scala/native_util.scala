import java.awt.image._
import com.googlecode.javacv.cpp.opencv_contrib._
import com.googlecode.javacv.cpp.opencv_core._

case class CVRect(val x: Int, val y: Int, val width: Int, val height: Int)

object NativeUtil {
  def initNativeInterface = {
    val libraryPath = "/u/echristiansen/Dropbox/head_segmentation/SFSPipeline/src/foreign_bin";
    System.setProperty("jna.library.path", libraryPath);
  }

  def cvLBPHDistance(leftPath: String, rightPath: String): Double = {
    println("in LBPH")
    println(leftPath)
    println(rightPath)
    val distance = NativeInterface.OpenCV.INSTANCE.lbphDistance(leftPath, rightPath)
    println(distance)
    distance
  }
  
  def colorToGray(color: BufferedImage): BufferedImage = {
    val gray = new BufferedImage(color.getWidth, color.getHeight, BufferedImage.TYPE_BYTE_GRAY)
    val graphics = gray.getGraphics
    graphics.drawImage(color, 0, 0, null);  
    graphics.dispose
    gray
  }

  def cvLBPHDistanceImage(leftColor: BufferedImage, rightColor: BufferedImage): Double = {
    val left = colorToGray(leftColor)
    val right = colorToGray(rightColor)
    val recognizer = createLBPHFaceRecognizer(1, 8, 8, 8, Double.MaxValue)
    val trainImages: MatVector = new MatVector(1)
    trainImages.put(0, new CvMat(IplImage.createFrom(left)))
    val trainLabels = CvMat.create(1, 1, CV_32SC(1))
    trainLabels.put(0, 0, 42)
    recognizer.get.train(trainImages, trainLabels)
    val label = Array(-1)
    val distance = Array(-1.0)
    recognizer.get.predict(new CvMat(IplImage.createFrom(right)), label, distance)
    distance(0)
  }

  def cvDetectFace(imagePath: String): Option[CVRect] = {
    val box = Array.fill(4)(0)
    NativeInterface.OpenCV.INSTANCE.detectFace(imagePath, box)
    if (box(0) != -1) Some(CVRect(box(0), box(1), box(2), box(3)))
    else None
  }
}
