package nebula

import java.awt.image.BufferedImage
import org.opencv.features2d.KeyPoint
import org.opencv.features2d.DescriptorExtractor
import org.opencv.core.Mat
import org.opencv.core.MatOfKeyPoint
import org.opencv.core.CvType
import grizzled.math._

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import util.imageProcessing.RichImage._

import graveyard._
import mpie._
import summary._
import smallBaseline._
import util._
import util.imageProcessing._
import wideBaseline._

///////////////////////////////////////////////////////////

// TODO: This design is weird. Is it right?
sealed trait Descriptor {
  val thisType: Manifest[_]

  def asType[E: Manifest]: E = {
    assert(thisType <:< implicitly[Manifest[E]])
    this.asInstanceOf[E]
  }

  def values[E: Manifest]: IndexedSeq[E] = {
    def helper[E: Manifest] = {
      val manifest = implicitly[Manifest[E]]
      this match {
        case d: SortDescriptor if implicitly[Manifest[Int]] <:< manifest =>
          d.values.asInstanceOf[IndexedSeq[E]]
        case d: RawDescriptor[_] if d.elementType <:< manifest =>
          d.values.asInstanceOf[IndexedSeq[E]]
        case _ => sys.error("failed match")
      }
    }

    val manifest = implicitly[Manifest[E]]
    // A hack to deal with Doubles.
    if (manifest == implicitly[Manifest[Double]]) {
      try {
        helper[Double].asInstanceOf[IndexedSeq[E]]
      } catch {
        case _ => helper[Int].map(_.toDouble).asInstanceOf[IndexedSeq[E]]
      }
    } else helper[E]
  }
}

///////////////////////////////////////////////////////////

case class RawDescriptor[E: Manifest](values: IndexedSeq[E]) extends Descriptor {
  val elementType = implicitly[Manifest[E]]
  override val thisType = implicitly[Manifest[RawDescriptor[E]]]
}

object RawDescriptor {
  implicit def toIndexedSeq[E](d: RawDescriptor[E]) = d.values
}

///////////////////////////////////////////////////////////

trait PermutationLike[A] {
  def invert: A
  def compose(otherPermutation: A): A
  def numCycles: Int
}

object PermutationLike {
  implicit def sortDescriptor(sort: SortDescriptor) = new PermutationLike[SortDescriptor] {
    override def invert = SortDescriptor.invert(sort)
    override def compose(otherPermutation: SortDescriptor) =
      SortDescriptor.compose(sort, otherPermutation)
    override def numCycles = SortDescriptor.numCycles(sort)
  }
}

///////////////////////////////////////////////////////////

case class SortDescriptor(values: IndexedSeq[Int]) extends Descriptor {
  assert(values.sorted == (0 until values.size))
  override val thisType = implicitly[Manifest[SortDescriptor]]
}

object SortDescriptor {
  implicit def toIndexedSeq(sort: SortDescriptor) = sort.values

  def fromUnsorted[A <% Ordered[A]](values: Seq[A]): SortDescriptor = {
    val permutation = values.zipWithIndex.sortBy(_._1).map(_._2)
    SortDescriptor(permutation.toIndexedSeq)
  }

  def invert(permutation: SortDescriptor): SortDescriptor = {
    val values = permutation.zipWithIndex.sortBy(_._1).map(_._2)
    SortDescriptor(values)
  }

  def compose(left: SortDescriptor, right: SortDescriptor): SortDescriptor = {
    val values = for (r <- right) yield left(r)
    SortDescriptor(values)
  }

  def numCycles(permutation: SortDescriptor): Int = {
    val seen = collection.mutable.Set[Int]()
    var numCycles = 0
    for (start <- permutation) {
      numCycles += { if (seen.contains(start)) 0 else 1 }
      var current = permutation(start)
      while (current != start) {
        seen += current
        current = permutation(current)
      }
    }
    numCycles
  }
}