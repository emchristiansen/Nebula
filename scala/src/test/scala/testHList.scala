import org.scalatest.FunSuite
import nebula._
import shapeless._

class TestHList extends FunSuite {
  test("cartesian product of HLists") {
    val list1 = 1 :: "one" :: HNil
    val list2 = 2 :: "two" :: HNil

    type Result1 = (Int, Int) :: (Int, String) :: (String, Int) :: (String, String) :: HNil
    val out1: Result1 = HListUtils.mkTuple2(list1, list2)
    val out1Golden = (1, 2) :: (1, "two") :: ("one", 2) :: ("one", "two") :: HNil
    assert(out1 === out1Golden)
  }
  
  test("cartesian product of 3 HLists") {
    val list1 = 1 :: HNil
    val list2 = 2.0 :: HNil
    val list3 = 3 :: "three" :: HNil
    type Result2 = ((Int, Double), Int) :: ((Int, Double), String) :: HNil 
    val out2: Result2 = HListUtils.mkTuple3(list1, list2, list3)
  }
}