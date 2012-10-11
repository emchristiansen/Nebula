import org.scalacheck.Prop.{ forAll, propBoolean }
import org.scalacheck.Properties
import org.scalatest.FunSuite
import nebula.{ CorrespondenceExperiment, FASTDetector, L0Matcher, SortExtractor, Util }
import nebula.Util.{ allSorts, prefixes }
import nebula.Util._

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck._
import nebula._
import MatcherParameterized._
import util._
import scala.util.Random

case class Person(firstName: String, lastName: String, int: Int, double: Double)

class TestMisc extends FunSuite {
  test("caseClassToStringMap") {
    val person = Person("Arthur", "Dent", 42, 3.14)

    val map = Util.caseClassToStringMap(person)
    val goldenMap = Map(
      "jsonClass" -> "Person",
      "firstName" -> "Arthur",
      "lastName" -> "Dent",
      "int" -> "42",
      "double" -> "3.14")

    assert(map === goldenMap)

    val experiment1 = CorrespondenceExperiment(
      "bikes",
      2,
      FASTDetector(100),
      SortExtractor(false, false, 8, 5, "Gray"),
      L0Matcher())

    val experiment2 = experiment1.copy(otherImage = 4)
    // val title = SummaryUtil.tableTitle(Seq(experiment1, experiment2))
    // val golden = "D-FASTDetector-MKP-100_E-SortExtractor-BW-5-C-true-NR-false-NS-false-PW-8_IC-bikes_M-L0Matcher_OI-*"
    // assert(title === golden)
  }

  test("numToBits") {
    assert(Util.numToBits(3)(15) === Seq(true, true, true))
    assert(Util.numToBits(5)(15) === Seq(false, true, true, true, true))
    assert(Util.numToBits(0)(12) === Seq())
    assert(Util.numToBits(5)(10) === Seq(false, true, false, true, false))
  }

  test("nonDistinctPermutations") {
    assert(nonDistinctPermutations(List(2, 1, 3)).size == List(2, 1, 3).permutations.size)
    assert(nonDistinctPermutations(List(1, 1)) == Seq(Seq(1, 1), Seq(1, 1)))
  }

  test("allSorts") {
    val hasUniqueSort = List(2, 1, 3, 4)

    assert(allSorts(hasUniqueSort) === List(List(1, 0, 2, 3)))

    assert(allSorts(List(2, 2, 4, 3)) === List(List(0, 1, 3, 2), List(1, 0, 3, 2)))
  }
  
  test("group") {
    assert(Util.group(List()) === List())
    
    assert(Util.group(List(1, 2, 3)) === List(List(1), List(2), List(3)))
    
    assert(Util.group(List(2, 2, 1, 3, 3, 2, 2, 2)) === 
      List(List(2, 2), List(1), List(3, 3), List(2, 2, 2)))
  }
  
  test("groupBySizes") {
    assert(Util.groupBySizes(List(), List()) === List())
    
    assert(Util.groupBySizes(List(2, 1, 2), List(1, 2, 3, 4, 5)) ===
      List(List(1, 2), List(3), List(4, 5)))
  }
}

object CheckMisc extends Properties("Util") {
  property("num prefixes same as num tails") = forAll {
    list: List[Int] => list.tails.size == prefixes(list).size
  }

  val randomPermutation = for (
    list <- Arbitrary.arbitrary[List[Int]]
  ) yield SortDescriptor.fromUnsorted(list)

  implicit lazy val arbitraryPermutation: Arbitrary[SortDescriptor] =
    Arbitrary(randomPermutation)

  property("numTranspositionsToSort is general Cayley") = forAll {
    s: SortDescriptor =>
      numTranspositionsToSort(s.values) == cayley(
        SortDescriptor(0 until s.values.size),
        s)
  }
  
  property("generalized Cayley is 'covariant' with permutations") = forAll {
    (seed: Int, leftLong: List[Int], rightLong: List[Int]) => {
      val (left, right) = leftLong.zip(rightLong).toIndexedSeq.unzip
      val leftPermuted = new Random(seed).shuffle(left)
      val rightPermuted = new Random(seed).shuffle(right)
//      if (generalizedCayley(RawDescriptor(left), RawDescriptor(right)) !=
//        generalizedCayley(RawDescriptor(leftPermuted), RawDescriptor(rightPermuted))) {
////        println(left.size)
////        println(right)
////        println(leftPermuted)
////        println(rightPermuted)
//        sys.error("asdf")
//      }
      generalizedCayley(RawDescriptor(left), RawDescriptor(right)) ==
        generalizedCayley(RawDescriptor(leftPermuted), RawDescriptor(rightPermuted))
    }
  }
}