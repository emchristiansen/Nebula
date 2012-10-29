package nebula

//// TODO: Test if these imports do anything.
//import mpie._
//import smallBaseline._
//import summary._
//import util._
//import util.imageProcessing._
//import wideBaseline._

///////////////////////////////////////////////////////////

package object nebula {
//  // TODO: Test if these imports do anything.
//  import mpie._
//  import smallBaseline._
//  import summary._
//  import util._
//  import util.imageProcessing._
//  import wideBaseline._

  implicit def intTimes(int: Int) = new {
    def times[A](function: => A): IndexedSeq[A] =
      (0 until int).map(_ => function)
  }

  def TODO = sys.error("TODO")
}