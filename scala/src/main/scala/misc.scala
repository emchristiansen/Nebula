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

object Global {
  val homeDirectory = System.getProperty("user.home")

  var run: RuntimeConfig = _

  val random = new Random(0)

  def loadRuntimeConfig(path: String) {
    run = RuntimeConfig.fromFile(path)
    println(run)
  }
}

case class MPIEProperties(val id: String, val session: String, val expression: String, val pose: String, val illumination: String) { 
  assert(id.size == 3)
  assert(List("01", "02", "03", "04").contains(session))
  assert(expression == "01")
  assert(List("240", "190", "051").contains(pose))
  assert(List("00", "04", "06").contains(illumination))

  def pathSegment = { 
    val poseUnderscore = pose match { 
      case "240" => "24_0"
      case "190" => "19_0"
      case "051" => "05_1"
    }
    "session%s/multiview/%s/%s/%s".format(session, id, expression, poseUnderscore)
  }
}

object MPIEProperties { 
  def parseMPIEPath(path: String): MPIEProperties = { 
    val filename = FilenameUtils.removeExtension((new File(path)).getName)
    val Parser = """(\S+)_(\S+)_(\S+)_(\S+)_(\S+)""".r
    val Parser(id, session, expression, pose, illumination) = filename
    MPIEProperties(id, session, expression, pose, illumination)
  }
}

object AtomicIO {
  val lockPath = "/tmp/eric_demo_lock"

  private def getLock(stream: FileOutputStream): java.nio.channels.FileLock = {
    try {
      stream.getChannel.lock()
    } catch {
      case e: java.nio.channels.OverlappingFileLockException => {
	Thread.sleep(50)
	getLock(stream)
      }
    }
  }

  def readImage(file: File): BufferedImage = {
    val foStream = new FileOutputStream(new File(lockPath))
    val lock = getLock(foStream)

    val image = ImageIO.read(file)

    lock.release()
    foStream.close()
    image
  }

  def writeImage(image: BufferedImage, formatName: String, file: File) {
    val foStream = new FileOutputStream(new File(lockPath))
    val lock = getLock(foStream)

    ImageIO.write(image, formatName, file)

    lock.release()
    foStream.close()
  }
}

object Geometry {
  def fitAffine(source: List[Point2D], target: List[Point2D]): AffineTransform = {
    assert(source.size == target.size)
    assert(source.size >= 3)
    
    val lhs = {
      val data = target.map(_.toList).transpose.map(_.toArray).toArray
      new Array2DRowRealMatrix(data).transpose
    }
    
    val rhs = {
      val data = source.map(_.toListHomogeneous).transpose.map(_.toArray).toArray
      new Array2DRowRealMatrix(data).transpose
    }
    
//    println("fitaffine")
//    println(lhs)
//    println(rhs)
    
    val solver = new SingularValueDecomposition(rhs).getSolver
    val transformation = solver.solve(lhs).transpose
    
//    println(transformation)
    
    assert(transformation.getRowDimension == 2)
    assert(transformation.getColumnDimension == 3)
    
    new AffineTransform(transformation.getData.transpose.flatten)
  }
}

object IO {
  def ObjectToByteArray[A](obj: A): Array[Byte] = {
    val byte_stream = new ByteArrayOutputStream
    (new ObjectOutputStream(byte_stream)).writeObject(obj)
    byte_stream.toByteArray
  }

  def ByteArrayToObject[A](byte_array: Array[Byte])(implicit m: Manifest[A]): A = {
    val obj = new ObjectInputStream(new ByteArrayInputStream(byte_array)) readObject

    obj match {
      case a if m.erasure.isInstance(a) => a.asInstanceOf[A]
      case _ => { sys.error("Type not what was expected when reading from file") }
    }
  }

  def WriteObjectToFile[A](obj: A, file: File) {
    val output = new ObjectOutputStream(new FileOutputStream(file, false))
    output.writeObject(obj)
    output.close()
  }

  def WriteObjectToFilename[A](obj: A, filename: String) = WriteObjectToFile(obj, new File(filename))

  def ReadObjectFromFile[A](file: File)(implicit m: Manifest[A]): A = {
    val obj = new ObjectInputStream(new FileInputStream(file)) readObject

    obj match {
      case a if m.erasure.isInstance(a) => a.asInstanceOf[A]
      case _ => { sys.error("Type not what was expected when reading from file") }
    }
  }

  def ReadObjectFromFilename[A](filename: String)(implicit m: Manifest[A]) = ReadObjectFromFile[A](new File(filename))
  
}

object Util { 
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

  def runSystemCommand(command: String): String = {
    println("running system command: %s".format(command))
    try {
      val out = sys.process.Process(command).!!
      println("successfully ran system command")
      out
    } catch {
      case e: Exception => throw new Exception("system command failed: %s\nException was %s\n".format(command, e.toString))
    } 
  }

  def runSystemCommandWaitFile(executable: String, path: String) {
    println("running system command: %s".format(executable))
    java.lang.Runtime.getRuntime.exec(executable)
//    sys.process.Process(command).run()
    println("waiting for file: %s".format(path))
    while (!(new File(path).exists)) Thread.sleep(100) // 100 milliseconds
  }

  def parallelize[A](seq: Seq[A]) = {
    if (Global.run.parallel) seq.par else seq
  }

  def createTempFile(prefix: String, suffix: String) = {
    val file = {
      if (Global.run.tempDirectory.size == 0) File.createTempFile(prefix, suffix)
      else File.createTempFile(prefix, suffix, new File(Global.run.tempDirectory))
    }
    if (Global.run.deleteTemporaryFiles) file.deleteOnExit
    file
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

  // TODO: Unit test this beast.
  def fitSimilarity(xsList: List[Tuple2[Double, Double]], ysList: List[Tuple2[Double, Double]]): List[List[Double]] = {
    // Calcuates similarity using method of 
    // "Least-Squares Estimation of Transformation Parameters Between To Point Patterns" by Umeyama.
    // Returns 3x3 homogeneous transformation matrix.

    assert(xsList.size == ysList.size && xsList.size >= 2)

    def vectorize(v: Tuple2[Double, Double]): RealVector = {
      val (v1, v2) = v
      new ArrayRealVector(Array(v1, v2))
    }

    val xs = xsList.map(vectorize)
    val ys = ysList.map(vectorize)

    def mean(vs: List[RealVector]): RealVector = {
      vs.reduce((l, r) => l.add(r)).mapMultiply(1.0 / vs.size.toDouble)
    }

    val muX = mean(xs)
    val muY = mean(ys)

    val centeredXs = xs.map(_.subtract(muX))
    val centeredYs = ys.map(_.subtract(muY))

    def covariance(v1s: List[RealVector], v2s: List[RealVector]): RealMatrix = {
      // Assume entries are centered.
      val outers = for ((v1, v2) <- v1s.zip(v2s)) yield v1.outerProduct(v2)
      outers.reduce((l, r) => l.add(r)).scalarMultiply(1.0 / v1s.size.toDouble)
    }

    val sigmaXSquared = covariance(centeredXs, centeredXs).getTrace
    val sigmaYSquared = covariance(centeredYs, centeredYs).getTrace
    val SigmaXY = covariance(centeredXs, centeredYs)

    val S = {
      val S = MatrixUtils.createRealIdentityMatrix(2)
      val determinant = new LUDecomposition(SigmaXY, 0).getDeterminant
      if (determinant < 0) {
	S.setEntry(1, 1, -1)
      }
      S
    }

    val svd = new SingularValueDecomposition(SigmaXY)

    val R = svd.getU.multiply(S.multiply(svd.getVT))
    val c = 1 / sigmaXSquared * svd.getS.multiply(S).getTrace
    val t = {
      val projected = R.operate(muX).mapMultiply(c)
      muY.subtract(projected)
    }

    val cR = R.scalarMultiply(c)

    val row0 = List(cR.getEntry(0, 0), cR.getEntry(0, 1), t.getEntry(0))
    val row1 = List(cR.getEntry(1, 0), cR.getEntry(1, 1), t.getEntry(1))
    val row2 = List(0.0, 0.0, 1.0)
    List(row0, row1, row2)
  }
}
