package nebula

import java.io.File
import org.apache.commons.io.FilenameUtils
import org.apache.commons.math3.linear._

import java.awt.geom._
import java.awt.image._
import java.awt.image.AffineTransformOp._

import util.Random

import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream

import javax.imageio.ImageIO

import com.googlecode.javacv.cpp.opencv_features2d._

object Global {
  val random = new Random(0)
  val homeDirectory = System.getProperty("user.home")
  var runVar: Option[RuntimeConfig] = None
  // TODO: Make this play well with various runtime config types.
  def run: RuntimeConfig = runVar match {
    case Some(config) => config
    case None => throw new Exception("Global.run not initialized")
  }
}

object Util { 
  def pruneKeyPoints(
    leftImage: BufferedImage, 
    rightImage: BufferedImage, 
    homography: Homography, 
    leftKeyPoints: List[KeyPoint]): List[Tuple2[KeyPoint, KeyPoint]] = {
    val leftWidth = leftImage.getWidth
    val leftHeight = leftImage.getHeight
    val insideLeft = 
      leftKeyPoints.filter(KeyPointUtil.isWithinBounds(leftWidth, leftHeight))

    val rightKeyPoints = insideLeft.map(homography.transform)

    val rightWidth = rightImage.getWidth
    val rightHeight = rightImage.getHeight
    for ((left, right) <- leftKeyPoints.zip(rightKeyPoints)
         if KeyPointUtil.isWithinBounds(rightWidth, rightHeight)(right)) yield (left, right) 
  }

  def imageWidthAndHeight(path: String): Tuple2[Int, Int] = {
    val command = "identify %s".format(path)

    val stdout = IO.runSystemCommand(command)
    val Parser = """(\d+)x(\d+)""".r
    val Parser(width, height) = stdout.split(" ")(2)
    (width.toInt, height.toInt)
  }

  def linesFromFile(file: File): List[String] = {
    io.Source.fromFile(file).mkString.split("\n").map(_.trim).filter(_.size > 0).filter(_.take(2) != "//").toList
  }

  def assertWithValue[A](assertion: () => Boolean, value: A): A = {
    assert(assertion())
    value
  }

  def allCombinations[T](listList: List[List[T]]): List[List[T]] = {
    listList match {
      case head :: tail => for (h <- head; t <- allCombinations(tail)) yield h :: t
      case _ => List(List[T]())
    }
  }

  def assertEQ[A](left: A, right: A) {
    assert(left == right, "%s == %s".format(left, right))
  }
  
  def assertContentsEqual[A](left: Seq[A], right: Seq[A]) {
    assertEQ(left.size, right.size)
    for ((l, r) <- left.view.zip(right.view)) {
      assertEQ(l, r)
    }
  }
  
  // Replace "Hi my name is ${name}." with "Hi my name is Eric."
  def bashStyleReplace(substitutions: Map[String, String], original: String): String = {
    def substituteSingle(string: String, keyAndReplacement: Tuple2[String, String]): String = {
      string.replace("${" + keyAndReplacement._1 + "}", keyAndReplacement._2)
    }
    
    substitutions.foldLeft(original)(substituteSingle)
  }
  
//  implicit def intTimes(i: Int) = new {
//    def times(fn: => Unit) = (1 to i) foreach (x => fn)
//  }  
  
  // def imageToArray(image: BufferedImage): Array2DRealRowMatrix = {
  //   val array = new Array2DRealRowMatrix(image.getHeight, image.getWidth)
  //   for (i <- 0 until image.getHeight; j <- 0 until image.getWidth) {
  //     array.setEntry(i, j, 
  // }

  def nextPowerOfTwo(n: Double): Int = {
    math.pow(2, (math.log(n) / math.log(2)).ceil).round.toInt
  }
  
  def countSort(input: Seq[Int], min: Int, max: Int): List[Int] = {
    input.foldLeft(Array.fill(max - min + 1)(0)) {
      (array, n) => array(n - min) += 1
      array
    }.zipWithIndex.foldLeft(List[Int]()) {
    case (lst, (cnt, ndx)) => List.fill(cnt)(ndx + min) ::: lst
  }.reverse
  }
  
  def permutation(input: Array[Int], max: Int): Array[Int] = {
    val histogram = Array.fill(max + 1)(List[Int]())
    input.zipWithIndex.foreach({x => histogram(x._1) = x._2 :: histogram(x._1)})
    val out = histogram.map(_.reverse).flatten
    assert(out.toSet == (0 until input.size).toSet)
    out
  }
  
  def recursiveListFiles(f: File): List[File] = { 
    if (!f.exists) throw new Exception("path does not exists: %s".format(f.toString))

    val these = f.listFiles.toList
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  def parallelize[A](seq: Seq[A]) = {
    if (Global.run.parallel) seq.par else seq
  }

  def truncate(list: List[Double]): String = { 
    list.map(l => "%.4f".format(l)).mkString(" ")
  }

  def gaussianKernel(std: Double): List[List[Double]] = {
    val width = (4 * std).ceil.toInt
    
    def density(pixel: Int): Double = {
      val normalizer = 1 / (std * math.sqrt(2 * math.Pi))

      val middle = (width - 1).toDouble / 2
      val unnormalizedDensity = math.exp(-math.pow(pixel - middle, 2) / (2 * math.pow(std, 2)))

      normalizer * unnormalizedDensity
    }

    // Isotropic Gaussians decompose.
    val projection = (for (i <- 0 until width) yield density(i)) toList

    for (p1 <- projection) yield {
      for (p2 <- projection) yield {
	p1 * p2
      }
    }
  }
}
