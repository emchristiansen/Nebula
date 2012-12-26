package nebula.util

import nebula.graveyard._
import nebula.mpie._
import nebula.summary._
import nebula.smallBaseline._
import nebula.util._
import nebula.util.imageProcessing._
import nebula.wideBaseline._
import nebula._

import java.awt.image.BufferedImage
import java.awt.image.BufferedImage._
import java.io.File

import scala.Array.canBuildFrom
import scala.text.{ DocText, Document }
import scala.util.Random

import org.opencv.features2d.KeyPoint

import nebula._
//import net.liftweb.json.{ JField, JObject, JString, Serialization }
//import net.liftweb.json.{ ShortTypeHints, parse, render }
//import net.liftweb.json.Serialization.write

import breeze.linalg._

import RichImage._
import MathUtil._
import nebula.util._

import reflect._

///////////////////////////////////////////////////////////

object DenseMatrixUtil {
  implicit class SeqSeqToDenseMatrix[A: ClassTag](seqSeq: IndexedSeq[IndexedSeq[A]]) {
    def toMatrix: DenseMatrix[A] = {
      val rows = seqSeq.size
      val cols = seqSeq.head.size
      for (row <- seqSeq) assert(row.size == cols)

      val matrix = new DenseMatrix[A](rows, cols)
      for (i <- 0 until rows; j <- 0 until cols) {
        matrix(i, j) = seqSeq(i)(j)
      }
      matrix
    }
  }

  implicit class DenseMatrixToSeqSeq[A](matrix: DenseMatrix[A]) {
    def toSeqSeq: IndexedSeq[IndexedSeq[A]] =
      for (i <- 0 until matrix.rows) yield {
        for (j <- 0 until matrix.cols) yield matrix(i, j)
      }
  }  
  
//  implicit def implicitDenseMatrixToSeqSeq[A](matrix: DenseMatrix[A]) = new {
//    def toSeqSeq: IndexedSeq[IndexedSeq[A]] =
//      for (i <- 0 until matrix.rows) yield {
//        for (j <- 0 until matrix.cols) yield matrix(i, j)
//      }
//  }

  implicit class DenseMatrixMethods[A: ClassTag](matrix: DenseMatrix[A]) {
    def rollVertical(deltaY: Int): DenseMatrix[A] =
      matrix.mapPairs({
        case ((y, x), value) => matrix((y - deltaY) mod matrix.rows, x)
      })
  }

  implicit class DenseMatrixTo(self: DenseMatrix[Int]) {
    //    // Simply dumps the Ints into the BufferedImage, with no concern
    //    // as to the resulting colors.
    //    def toImage: BufferedImage = {
    //      val image = new BufferedImage(self.cols, self.rows, TYPE_INT_ARGB)
    //      for (y <- 0 until self.rows; x <- 0 until self.cols) {
    //        image.setRGB(x, y, self(y, x))
    //      }
    //      image
    //    }

    def toImage: BufferedImage = {
      val image = new BufferedImage(self.cols, self.rows, TYPE_INT_ARGB)
      for (y <- 0 until self.rows; x <- 0 until self.cols) {
        val value = self(y, x)
        assert(value >= 0 && value < 256)

        val pixel = Pixel(255, value, value, value)
        image.setRGB(x, y, pixel.argb)
      }
      image
    }
  }

  implicit class DenseMatrixDoubleTo(self: DenseMatrix[Double]) {
    def toScaledImage: BufferedImage = {
      val translated = self - self.min
      val scaled = (translated / translated.max).map(_ * 255).map(_.round.toInt)
      scaled.toImage
    }
  }

  implicit class AddScaled(self: DenseMatrix[Int]) {
    def toScaledImage: BufferedImage = self.map(_.toDouble).toScaledImage
  }

  implicit class BufferedImageToDenseMatrix(self: BufferedImage) {
    def toMatrix: DenseMatrix[Int] = {
      val matrix = new DenseMatrix[Int](self.getHeight, self.getWidth)
      for (y <- 0 until self.getHeight; x <- 0 until self.getWidth) {
        val pixel = self.getPixel(x, y)
        matrix(y, x) = pixel.gray.head
      }
      matrix
    }
  }
}

///////////////////////////////////////////////////////////

object Util extends Logging {
  def sortMap[A, B](map: Map[A, B])(implicit ordering: Ordering[A]): collection.immutable.SortedMap[A, B] = {
    val treeMap = new collection.immutable.TreeMap[A, B]
    map.toList.foldLeft(treeMap) {
      case (treeMap, pair) => treeMap + pair
    }
  }
  
  def groupBySizes[A](sizes: Seq[Int], seq: Seq[A]): Seq[Seq[A]] = {
    assert(sizes.sum == seq.size)
    if (!sizes.isEmpty) assert(sizes.min > 0)

    if (sizes.isEmpty) {
      List()
    } else {
      Seq(seq.take(sizes.head)) ++ groupBySizes(sizes.tail, seq.drop(sizes.head))
    }
  }

  // Groups consecutive identical elements into the same sublists.
  def group[A](seq: List[A]): List[List[A]] = {
    if (seq.isEmpty) List()
    else {
      seq.tail.foldLeft(List(List(seq.head)))((left, right) => {
        if (left.head.head == right) (right :: left.head) :: left.tail
        else List(right) :: left
      }).reverse
    }
  }

  def numTranspositionsToSort[A <% Ordered[A]](seq: Seq[A]): Int = {
    if (seq.isEmpty) 0
    else {
      val (minElement, minIndex) = seq.zipWithIndex.minBy(_._1)
      if (minElement == seq.head) numTranspositionsToSort(seq.tail)
      else {
        1 + numTranspositionsToSort(seq.tail.updated(minIndex - 1, seq.head))
      }
    }
  }

  // Like |tails|, but prefixes instead.
  // This implementation seems a bit weird to me.
  def prefixes[A](seq: Seq[A]): Seq[Seq[A]] =
    seq.reverse.tails.toSeq.map(_.reverse).reverse

  // Like list.permutations, but doesn't cull repeated elements.
  def nonDistinctPermutations[A](seq: Seq[A]): Seq[Seq[A]] = {
    val indexedSeq = seq.toIndexedSeq
    for (permutation <- (0 until indexedSeq.size).permutations.toSeq) yield {
      permutation.map(i => indexedSeq(i))
    }
  }

  // Return all legal sorts of the given list.
  def allSorts[T <% Ordered[T]](values: Seq[T]): Seq[Seq[Int]] = {
    def helper(withIndex: Seq[Tuple2[T, Int]]): Seq[Seq[Tuple2[T, Int]]] = {
      if (withIndex.isEmpty) List(Nil)
      else {
        val smaller = withIndex.filter(_._1 < withIndex.head._1)
        val equal = withIndex.filter(x => !(x._1 < withIndex.head._1) && !(x._1 > withIndex.head._1))
        val bigger = withIndex.filter(_._1 > withIndex.head._1)
        assert(smaller.size + equal.size + bigger.size == withIndex.size)
        for (
          smallerSort <- helper(smaller);
          equalSort <- equal.permutations;
          biggerSort <- helper(bigger)
        ) yield {
          smallerSort ++ equalSort ++ biggerSort
        }
      }
    }

    helper(values.zipWithIndex).map(_.map(_._2))
  }

  // Assuming |num| is represented as an arbitrarily long unsigned int, get
  // the |numBits| low order bits in the representation, where the head of
  // the list is the high-order bit.
  def numToBits(numBits: Int)(num: Int): Seq[Boolean] = {
    require(numBits >= 0)
    require(num >= 0)

    val divisors = (0 until numBits).reverse.map(p => math.pow(2, p).toInt)
    val divided = divisors.map(d => num / d)
    divided.map(_ % 2 == 1)
  }

//  // Takes a set of pairs and ensures they form an onto function.
//  // When onto-ness is violated, only the best pair is kept.
//  // The best pair is the one that comes earliest in the ordering.
//  def makeOntoFunction[A, B](pairs: Seq[Tuple2[A, B]])(
//      implicit ordering: Ordering[Tuple2[A, B]]): Set[Tuple2[A, B]] = {
//	// Take the first duplicate first entries.
//    val pairs1 = pairs.groupBy(_._1).map(_._2).map(_.head)
//    
//    // Remove all pairs with duplicate second entries.
//    pairs1.groupBy(_._2).filter(_._2.size > 1).map(_._2).map(_.head).toSet
//  } 
  
  // Warp the leftKeyPoint by the homography and return its nearest neighbor
  // among the rightKeyPoints.
  def nearestUnderWarp(
    threshold: Double,
    homography: Homography,
    rightKeyPoints: Seq[KeyPoint])(leftKeyPoint: KeyPoint): Option[KeyPoint] = {
    val leftWarped = KeyPointUtil.transform(homography)(leftKeyPoint)
    val rightWithDistances = rightKeyPoints zip rightKeyPoints.map(
      right => KeyPointUtil.euclideanDistance(leftWarped, right))
    val (nearest, distance) = rightWithDistances.minBy(_._2)
    if (distance < threshold) Some(nearest)
    else None
  }

  // Find the nearest neighbor for every leftKeyPoint and return the sequence
  // of pairs. Remove pairs where the rightKeyPoint isn't found. When multiple
  // leftKeyPoints pair with the same rightKeyPoint, take only the pair with
  // the best leftKeyPoint.
  def nearestUnderWarpRemoveDuplicates(
    threshold: Double,
    homography: Homography,
    leftKeyPoints: Seq[KeyPoint],
    rightKeyPoints: Seq[KeyPoint]): Seq[Tuple2[KeyPoint, KeyPoint]] = {
    require(leftKeyPoints.size == leftKeyPoints.toSet.size)
    require(rightKeyPoints.size == rightKeyPoints.toSet.size)
    // TODO
    logDebug("leftKeyPoints.size: " + leftKeyPoints.size)
    logDebug("rightKeyPoints.size: " + rightKeyPoints.size)
    println("leftKeyPoints.size: " + leftKeyPoints.size)
    println("rightKeyPoints.size: " + rightKeyPoints.size)    
    val rightMatches = leftKeyPoints.map(nearestUnderWarp(
      threshold,
      homography,
      rightKeyPoints))
    assert(leftKeyPoints.size == rightMatches.size)
    val culledOption = leftKeyPoints zip rightMatches filter (_._2.isDefined)
    val culled = culledOption map {
      case (left, rightOption) => (left, rightOption.get)
    }
    logDebug("culled.size: " + culled.size)
    // TODO
    println("culled.size: " + culled.size)
    
    // Sort all the putative pairs by their quality.
    // The best pairs will be _at the end_.
    val sorted = culled.sortBy(KeyPointUtil.pairQuality)
    
//    // Use the Map constructor to quickly filter out repeated keys
//    // and repeated values.
//    // Note the constructor favors later key-value pairs over
//    // earlier pairs, so it will take the pairs with the highest quality.
//    val noKeyDuplicates = sorted.toMap.toSeq.sortBy(KeyPointUtil.pairQuality)
    
    // Now the tricky bit; we flip the map, making keys into values and
    // vice-versa. We then repeat the constructor trick, this time removing
    // repeated values.
    val noDuplicates = sorted.map(_.swap).toMap.toSeq.map(_.swap)
    
    // Now sort the results so the best matches come first.
    noDuplicates.sortBy(KeyPointUtil.pairQuality).reverse
    
//    val onto = makeOntoFunction(culled).toIndexedSeq
//    logDebug("onto.size: " + onto.size)
//    // TODO
//    println("onto.size: " + onto.size)
//    onto
  }

  def pruneKeyPoints(
    leftImage: BufferedImage,
    rightImage: BufferedImage,
    homography: Homography,
    leftKeyPoints: Seq[KeyPoint]): Seq[Tuple2[KeyPoint, KeyPoint]] = {
    val leftWidth = leftImage.getWidth
    val leftHeight = leftImage.getHeight
    val insideLeft =
      leftKeyPoints.filter(KeyPointUtil.isWithinBounds(leftWidth, leftHeight))

    val rightKeyPoints = insideLeft.map(KeyPointUtil.transform(homography))

    val rightWidth = rightImage.getWidth
    val rightHeight = rightImage.getHeight
    for (
      (left, right) <- leftKeyPoints.zip(rightKeyPoints) if KeyPointUtil.isWithinBounds(rightWidth, rightHeight)(right)
    ) yield (left, right)
  }

  def imageWidthAndHeight(path: String): Tuple2[Int, Int] = {
    val command = "identify %s".format(path)

    val stdout = IO.runSystemCommand(command)
    val Parser = """(\d+)x(\d+)""".r
    val Parser(width, height) = stdout.split(" ")(2)
    (width.toInt, height.toInt)
  }

  def linesFromFile(file: File): Seq[String] = {
    scala.io.Source.fromFile(file).mkString.split("\n").map(_.trim).filter(_.size > 0).filter(_.take(2) != "//").toList
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
      (array, n) =>
        array(n - min) += 1
        array
    }.zipWithIndex.foldLeft(List[Int]()) {
      case (lst, (cnt, ndx)) => List.fill(cnt)(ndx + min) ::: lst
    }.reverse
  }

  // Uses a linear time algorithm, but is in fact quite a slow implementation.
  def permutation(input: Array[Int], max: Int): Array[Int] = {
    val histogram = Array.fill(max + 1)(List[Int]())
    input.zipWithIndex.foreach({ x => histogram(x._1) = x._2 :: histogram(x._1) })
    val out = histogram.map(_.reverse).flatten
    assert(out.toSet == (0 until input.size).toSet)
    out
  }

  def recursiveListFiles(f: File): Seq[File] = {
    if (!f.exists) throw new Exception("path does not exists: %s".format(f.toString))

    val these = f.listFiles.toList
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  //  // TODO: This doesn't actually work. It always returns what are effectively
  //  // serial collections.
  //  def parallelize[A](seq: Seq[A]): collection.GenSeq[A] = {
  //    if (Global.run[RuntimeConfig].parallel) seq.par else seq
  //  }

  def truncate(list: Seq[Double]): String = {
    list.map(l => "%.4f".format(l)).mkString(" ")
  }
}
