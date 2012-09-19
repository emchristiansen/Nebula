import org.scalatest.FunSuite

class TestAnything extends FunSuite {
  val source = """
  import nebula._

    val experiment1 = CorrespondenceExperiment[FASTDetector, SortDescriptor, SortExtractor, L0Matcher](
      "bikes", 
      2, 
      FASTDetector(100),
      SortExtractor(false, false, 8, 5, true),
      L0Matcher())
  experiment1
"""

  test("anything") {
    //    val x = DenseVector.zeros[Double](2)
    //    println(x)
    ////    val y = new DenseMatrix[Double](5, 4)
    //    val y = DenseMatrix.tabulate(4, 3)((i, j) => "hi")
    //    println(y)
  }
}
