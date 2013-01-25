import org.scalatest.FunSuite
import nebula._
import shapeless._

class TestHList extends FunSuite {
  test("cartesian product of HLists") {
    val list1 = 1 :: "one" :: HNil
    val list2 = 2 :: "two" :: HNil

    type Result = (Int, Int) :: (Int, String) :: (String, Int) :: (String, String) :: HNil
    val list3: Result = HListUtils.liftA2(tuple2)(list1, list2)
    val list3Golden = (1, 2) :: (1, "two") :: ("one", 2) :: ("one", "two") :: HNil
    assert(list3 === list3Golden)
  }
}