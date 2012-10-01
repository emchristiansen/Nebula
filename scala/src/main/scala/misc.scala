package nebula

import java.awt.image.BufferedImage
import java.io.File

import scala.Array.canBuildFrom
import scala.text.{ DocText, Document }
import scala.util.Random

import org.opencv.features2d.KeyPoint

import net.liftweb.json.{ JField, JObject, JString, Serialization }
import net.liftweb.json.{ ShortTypeHints, parse, render }
import net.liftweb.json.Serialization.write

object Global {
  val random = new Random(0)
  val homeDirectory = new File(System.getProperty("user.home"))
  var runVar: Option[RuntimeConfigTrait] = None
  // TODO: This asInstanceOf stuff has bad code smell.
  def run[R <: RuntimeConfigTrait](implicit manifest: Manifest[R]): R = runVar match {
    case Some(config) => {
      assert(manifest.erasure.isInstance(config))
      config.asInstanceOf[R]
    }
    case None => throw new Exception("Global.run not initialized")
  }
}

object Util {
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
    if (seq.isEmpty) List(Nil)
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

  // The correct implementation of a % b.
  def modulo(a: Double, b: Double): Double = a - (a / b).floor * b

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

  def caseClassToStringMap[A <: AnyRef](caseClass: A): Map[String, String] = {
    // Implementation uses lift-json for introspection, which is
    // admittedly roundabout. It is also likely brittle; I'm guessing it will
    // fail for nested structures.
    // TODO: Use introspection directly instead of lift-json.
    implicit val formats = Serialization.formats(ShortTypeHints(List(caseClass.getClass)))

    val string = write(caseClass)
    val json = parse(string)

    import scala.text.{ Document, DocText }
    def documentToString(document: Document): String = document match {
      case DocText(string) => string.replace("\"", "")
      case _ => throw new Exception
    }

    val JObject(jObject) = json
    val jStrings = jObject.map({ case JField(key, value) => JField(key, JString(documentToString(render(value)))) })
    val jsonString = JObject(jStrings)

    jsonString.extract[Map[String, String]]
  }

  def abbreviate[A <: AnyRef](caseClass: A): String = {
    // For a case class like
    // > case class AwesomeDetector(theFirstParameter: String, second: Int)
    // and 
    // > val ad = AwesomeDetector("helloWorld", 42)
    // produces an abbreviation like
    // "AwesomeDetector-TFP-helloWorld-S-42".
    val map = Util.caseClassToStringMap(caseClass)

    def camelCaseToAbbreviation(camelCase: String): String = {
      // Assume camelCase for parameter names. Otherwise
      // the abbreviations will be weird.
      // Example: "myCoolValue" becomes "MCV".
      camelCase.head.toUpper + camelCase.filter(_.isUpper)
    }

    val parameters = map.filterKeys(_ != "jsonClass").toList.sortBy(_._1)
    val parameterNames = parameters.map(p => camelCaseToAbbreviation(p._1))
    val parameterValues = parameters.map(p => p._2)

    val parameterParts = List(parameterNames, parameterValues).transpose.flatten
    val parts = map("jsonClass") :: parameterParts
    parts.mkString("-")
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

  // TODO: This doesn't actually work. It always returns what are effectively
  // serial collections.
  def parallelize[A](seq: Seq[A]): collection.GenSeq[A] = {
    if (Global.run[RuntimeConfig].parallel) seq.par else seq
  }

  def truncate(list: Seq[Double]): String = {
    list.map(l => "%.4f".format(l)).mkString(" ")
  }

  def gaussianKernel(std: Double): Seq[Seq[Double]] = {
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
