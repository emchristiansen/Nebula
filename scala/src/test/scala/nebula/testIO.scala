package nebula

import nebula.util._
import org.scalatest._
import org.scalatest.prop._
import javax.imageio.ImageIO
import java.io.File
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random
import breeze.linalg._

import org.scalacheck._
import breeze.math._

import org.apache.commons.math3.transform.DftNormalization
import org.apache.commons.math3.transform.FastFourierTransformer
import org.apache.commons.math3.transform.TransformType
import DenseMatrixUtil._
import reflect._

///////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
@WrapWith(classOf[ConfigMapWrapperSuite])
class TestIO(
  override val configMap: Map[String, Any]) extends StandardSuite {

  test("bz2", FastTest) {
    val file = File.createTempFile("testIO_bz2", ".bz2")

    val text = "hello" * 1000
    file.bz2WriteString(text)

    val compressedText = file.readString
    asserty(compressedText.size < 0.2 * text.size)

    val recoveredText = file.bz2ReadString
    asserty(text == recoveredText)
  }

  test("bz2 on String", FastTest) {
    forAll(Gen.listOf(Gen.alphaNumChar)) { chars =>
      val string = chars.mkString

      val compressed = string.compress

      val decompressed = compressed.decompress

      asserty(string == decompressed)
    }
  }
}