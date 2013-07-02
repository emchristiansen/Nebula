package nebula.graveyard
//
//import java.awt.Rectangle
//import java.awt.geom.AffineTransform
//import java.awt.image.AffineTransformOp
//import java.awt.image.AffineTransformOp.TYPE_BILINEAR
//import java.awt.image.BufferedImage
//import java.io.File
//import com.github.mhendred.face4j.DefaultFaceClient
//import com.github.mhendred.face4j.model.Face
//import com.github.mhendred.face4j.model.Photo
//import com.github.mhendred.face4j.model.Point
//import javax.imageio.ImageIO
//
//import nebula.graveyard._
//import nebula.mpie._
//import nebula.summary._
//import nebula.smallBaseline._
//import nebula.util._
//import nebula.imageProcessing._
//import nebula.wideBaseline._
//
case class Point2D(val x: Double, val y: Double) {
  def toList: List[Double] = List(x, y)
  def toListHomogeneous: List[Double] = List(x, y, 1)
}
//
//case class FaceComFiducials(val leftEye: Point2D,
//                            val rightEye: Point2D,
//                            val nose: Point2D,
//                            val mouthCenter: Point2D) {
//  def toList: List[Point2D] = List(leftEye, rightEye, nose, mouthCenter)
//}
//
//object FaceCom {
//  System.setProperty("log4j.defaultInitOverride", "true")
//
//  val API_KEY = "486a68f7113058df81c7f0841726f9a8"
//  val API_SECRET = "bd141f00cb8b9e5ce3f4e10c96b50b49"
//  val NAMESPACE = "wg"
//
//  lazy val face_client = {
//    val face_client = new DefaultFaceClient(API_KEY, API_SECRET)
//    face_client.setAggressive(true)
//    face_client
//  }
//
//  lazy val canonicalFaceBox: BufferedImage = {
//    val url = getClass.getResource("/average_male_small_HIE.bmp")
//    ImageIO.read(new File(url.getFile))    
//  }
//
//  val canonicalFile = new File(getClass.getResource("/average_male_small.jpg").getFile) 
//
//  lazy val canonicalImage = ImageIO.read(canonicalFile)
//
//  lazy val canonicalFiducials: FaceComFiducials = {
//    val photo = detect(canonicalFile)
//    fiducials(photo)
//  }
//
//  lazy val faceBox: Rectangle = {
//    val image = ImageIO.read(canonicalFile)
//    val midX = image.getWidth / 2
//    val midY = image.getHeight / 2
//    val yUp = 70
//    val yDown = 60
//    val xRadius = 40
//    new Rectangle(midX - xRadius, midY - yUp, 2 * xRadius + 1, yUp + yDown + 1)
//  }
//
//  def alignAndCropWithPhoto(image: BufferedImage, photo: Photo): BufferedImage = {
//    val warp = canonicalWarp(FaceCom.fiducials(photo))
//    val transformOp = new AffineTransformOp(warp, TYPE_BILINEAR)
//    val warped = transformOp.filter(image, null)
//    ImageUtil.extractROI(canonicalFaceBox, warped)
//  }
//
//  def alignAndCropImage(imageFile: File): Option[BufferedImage] = {
//    val image = ImageIO.read(imageFile)
//    val cacheFile = {
//      val hash = Pixel.getPixels(image).hashCode
//      new File("/tmp/debug/cacheAlign_%s.png".format(hash))
//    }
//
//    if (cacheFile.exists) {
//      println("using cached crop")
//      return Some(ImageIO.read(cacheFile))
//    } else {
//      return None
//    }
//
//    val photo = detect(imageFile)
//    if (FaceCom.goodPhoto(photo)) {
//      println("good image")
//      val cropped = alignAndCropWithPhoto(image, photo)
//      ImageIO.write(cropped, "png", cacheFile)
//      Some(cropped)
//    } else {
//      println("bad image")
//      None
//    }
//  }
//
//  def canonicalWarp(fiducials: FaceComFiducials): AffineTransform = {
//    Geometry.fitAffine(fiducials.toList, canonicalFiducials.toList)
//  }
//
//  def fiducials(photo: Photo): FaceComFiducials = {
//    assert(goodPhoto(photo))
//
//    def imageCoords(point: Point): Point2D = {
//      Point2D(point.x * photo.getWidth / 100.0, point.y * photo.getHeight / 100.0)
//    }
//
//    val face = photo.getFace
//    FaceComFiducials(imageCoords(face.getLeftEye),
//      imageCoords(face.getRightEye),
//      imageCoords(face.getNose),
//      imageCoords(face.getMouthCenter))
//  }
//
//  def fiducialsFromImage(image: BufferedImage): Tuple2[Photo, Option[FaceComFiducials]] = {
//    val tempPath = IO.createTempFile("demo", ".jpg")
//    ImageIO.write(image, "jpg", tempPath)
//    val photo = detect(tempPath)
//    if (goodPhoto(photo)) {
//      (photo, Some(fiducials(photo)))
//    } else {
//      (photo, None)
//    }
//  }
//
//  def goodFace(face: Face): Boolean = {
//    val confidence = face.getFaceConfidence > 90
//    val yaw = face.getYaw.abs < 20
//    val pitch = face.getPitch.abs < 20
//    confidence && yaw && pitch
//  }
//
//  def goodPhoto(photo: Photo): Boolean = {
//    if (photo.getFaceCount == 0) false
//    else goodFace(photo.getFace)
//  }
//
//  def detect(imageFile: File): Photo = {
//    face_client.detect(imageFile)
//  }
//
//  //  def Recognize(image_filename: String, scope: String): Photo = {
//  //    val image_file = new File(image_filename)
//  //    face_client.recognize(image_file, scope)
//  //  }
//  //
//  //  def WriteToFile(text: String, filename: String) {
//  //    val out = new BufferedWriter(new FileWriter(filename));
//  //    out.write(text);
//  //    out.close();
//  //  }
//  //
//  //  val FIDUCIALS = List("center", "left_eye", "right_eye", "mouth_left", "mouth_right", "mouth_center", "left_ear", "right_ear", "chin", "nos\
//  //e")
//  //  val GetTableTuple = {
//  //    val xs_and_ys = FIDUCIALS.flatMap(x => List(x + "_x", x + "_y"))
//  //    val with_type = xs_and_ys.map(_ + ": real")
//  //    "(" + with_type.reduce(_ + ", " + _) + ")"
//  //  }
//  //
//  //  def ImageNumFromFilename(filename: String): Int = filename.take(6).toInt
//  //
//  //  def IdentityFromFilename(filename: String): Int = {
//  //    val Identity = """.*identity-([0-9]+).*""".r
//  //    val Identity(identity) = filename
//  //    identity.toInt
//  //  }
//}
