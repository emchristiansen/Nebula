import org.scalatest.FunSuite

import nebula.SummaryUtil

class TestSummary extends FunSuite { 
  test("mapUnion") {
    val map1 = Map(1 -> 12, 2 -> 13)
    val map2 = Map(1 -> 10, 3 -> 10, 2 -> 13)
    val maps = Set(map1, map2)

    val union = SummaryUtil.mapUnion(maps)
    val golden = Map(1 -> Set(12, 10), 2 -> Set(13), 3 -> Set(10))

    assert(union === golden)
  }

  test("changingFields") {
    val map1 = Map(1 -> 10, 2 -> 20)
    val map2 = Map(1 -> 10, 2 -> 30)
    val maps = Seq(map1, map2)

    val changing = SummaryUtil.changingFields(maps)
    val golden = Seq(Map(2 -> 20), Map(2 -> 30))
    assert(changing === golden)
  }

  test("summarizeStructure") {
    val map1 = Map("a" -> "aa", "b" -> "bb")
    val map2 = Map("a" -> "aa", "b" -> "cc")
    val summary = SummaryUtil.summarizeStructure(Set(map1, map2))
    val golden = "a-aa_b-*"
    assert(summary === golden)
  }
}
