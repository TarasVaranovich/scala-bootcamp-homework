package basics

import basics.DataStructures.NotImplemented
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class DataStructuresSpec extends AnyFlatSpec {

  "Map(a -> 1, b -> 2, c -> 4, d -> 1, e -> 0, f -> 2, g -> 2)" should
    "result List(Set(e) -> 0, Set(a, d) -> 1, Set(b, f, g) -> 2, Set(c) -> 4)" in {

    DataStructures.sortConsideringEqualValues(
      Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)) shouldBe
      List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)
  }

  "Map(23.4 -> 10, 54.6 -> 237, 0.8 -> 5, 99.6 -> 237, 0.1 -> 10, 0.2 -> 2, 34.7 -> 10)" should
    "result List(Set(0.2) -> 2, Set(0.8) -> 5, Set(0.1, 23.4, 34.7) -> 10, Set(54.6, 99.6) -> 237)" in {

    DataStructures
      .sortConsideringEqualValues(
        Map(23.4 -> 10, 54.6 -> 237, 0.8 -> 5, 99.6 -> 237, 0.1 -> 10, 0.2 -> 2, 34.7 -> 10)) shouldBe
      List(Set(0.2) -> 2, Set(0.8) -> 5, Set(0.1, 23.4, 34.7) -> 10, Set(54.6, 99.6) -> 237)
  }

  "null" should "should throw exception" in {
    assertThrows[java.lang.NullPointerException](DataStructures.sortConsideringEqualValues(null))
  }

  "Map(NotImplemented(11) -> 2, NotImplemented(12) -> 2, NotImplemented(13) -> 3, NotImplemented(14) -> 3))" should
    "result List(Set(NotImplemented(11), NotImplemented(12)) -> 2, Set(NotImplemented(13), NotImplemented(14)) -> 3)" in {

    DataStructures.sortConsideringEqualValues(
      Map(NotImplemented(11) -> 2, NotImplemented(12) -> 2, NotImplemented(13) -> 3, NotImplemented(14) -> 3)) shouldBe
      List(Set(NotImplemented(11), NotImplemented(12)) -> 2, Set(NotImplemented(13), NotImplemented(14)) -> 3)
  }

  "list of maps treatment with case of empty map" should "be executed without exception" in {
    List(
      Map(23.4 -> 10, 54.6 -> 237, 0.8 -> 5, 99.6 -> 237, 0.1 -> 10, 0.2 -> 2, 34.7 -> 10),
      Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2),
      Map.empty
    ).foreach(map => DataStructures.sortConsideringEqualValues(map))
  }
}
