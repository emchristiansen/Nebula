package nebula

///////////////////////////////////////////////////////////

package object nebula {
  implicit def intTimes(int: Int) = new {
    def times[A](function: => A): IndexedSeq[A] = 
      (0 until int).map(_ => function)
  }
  
  def TODO = sys.error("TODO")
}