package nebula.graveyard

import nebula._

import org.opencv.core._
import org.opencv.contrib._
import org.opencv.highgui.Highgui._
import org.opencv.objdetect._

import java.awt.image._

object NativeUtil {
  def initNativeInterface = {
    sys.error("Are you sure you meant to use this?")
//    val libraryPath = ???/bin
//    System.setProperty("jna.library.path", libraryPath);
  }

  def cvLBPHDistance(leftPath: String, rightPath: String): Double = {
    sys.error("TODO")
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
    // The necessary classes aren't being generated currently.
    sys.error("TODO")
    //    val recognizer = createLBPHFaceRecognizer(1, 8, 8, 8, Double.MaxValue)
    //    val trainImages: MatVector = new MatVector(1)
    //    trainImages.put(0, new CvMat(IplImage.createFrom(left)))
    //    val trainLabels = CvMat.create(1, 1, CV_32SC(1))
    //    trainLabels.put(0, 0, 42)
    //    recognizer.get.train(trainImages, trainLabels)
    //    val label = Array(-1)
    //    val distance = Array(-1.0)
    //    recognizer.get.predict(new CvMat(IplImage.createFrom(right)), label, distance)
    //    distance(0)
  }

  def cvDetectFace(imagePath: String): Option[Rect] = {
    val image = imread(imagePath, IMREAD_GRAYSCALE)
    
    val cascade = new CascadeClassifier("haarcascade_frontalface_alt.xml")
    val detectionsMat = new MatOfRect
    cascade.detectMultiScale(image, detectionsMat)
    detectionsMat.toArray.toList.headOption
  }
}
