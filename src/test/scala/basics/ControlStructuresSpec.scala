package basics

import basics.ControlStructures.Command._
import basics.ControlStructures.process
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ControlStructuresSpec extends AnyFlatSpec {

  "divide 4 5" should "be 'Divide' command" in {
    ControlStructures.parseCommand("divide 4 5") shouldBe Right(Divide(4, 5))
  }

  "divide 4 0" should "be 'Left'" in {
    assert(ControlStructures.parseCommand("divide 4 0").isLeft)
  }

  "sum 5 5 6 8.5" should "be 'Sum' command" in {
    ControlStructures.parseCommand("sum 5 5 6 8.5") shouldBe Right(Sum(List(5, 5, 6, 8.5)))
  }

  "average 4 3 8.5 4" should "be 'Average' command" in {
    ControlStructures.parseCommand("average 4 3 8.5 4") shouldBe Right(Average(List(4, 3, 8.5, 4)))
  }

  "min 4 -3 -17" should "be 'Min' command" in {
    ControlStructures.parseCommand("min 4 -3 -17") shouldBe Right(Min(List(4, -3, -17)))
  }

  "max 4 -3 -17" should "be 'Max' command" in {
    ControlStructures.parseCommand("max 4 -3 -17") shouldBe Right(Max(List(4, -3, -17)))
  }

  "max 4 -3 -17 with extra spaces" should "be 'Max' command" in {
    ControlStructures.parseCommand(" max 4    -3 -17 ") shouldBe Right(Max(List(4, -3, -17)))
  }

  "max 4" should "be 'Left'" in {
    assert(ControlStructures.parseCommand("max 4").isLeft)
  }

  "max" should "be 'Left'" in {
    assert(ControlStructures.parseCommand("max4").isLeft)
  }

  "command" should "be 'Left'" in {
    assert(ControlStructures.parseCommand("command").isLeft)
  }

  "divide 4 5" should "be '4 divided by 5 is 0.8'" in {
    process("divide 4 5") shouldBe "4 divided by 5 is 0.8"
  }

  "divide 2 3" should "be '2 divided by 3 is 0.667'" in {
    process("divide 2 3") shouldBe "2 divided by 3 is 0.667"
  }

  "sum 5 5 6 8.5" should "be 'the sum of 5 5 6 8.5 is 24.5'" in {
    process("sum 5 5 6 8.5") shouldBe "the sum of 5 5 6 8.5 is 24.5"
  }

  "average 2 2 3" should "be 'the average of 2 2 3 is 2.333'" in {
    process("average 2 2 3") shouldBe "the average of 2 2 3 is 2.333"
  }

  "average 4 3 8.5 4" should "be 'the average of 4 3 8.5 4 is 4.875'" in {
    process("average 4 3 8.5 4") shouldBe "the average of 4 3 8.5 4 is 4.875"
  }

  "min 4 -3 -17" should "return 'the minimum of 4 -3 -17 is -17'" in {
    process("min 4 -3 -17") shouldBe "the minimum of 4 -3 -17 is -17"
  }

  "max 4 -3 -17" should "be 'the maximum of 4 -3 -17 is 4'" in {
    process("max 4 -3 -17") shouldBe "the maximum of 4 -3 -17 is 4"
  }

  "max 4.45005 -3.75 -17.875" should "be 'the maximum of 4.45005 -3.75 -17.875 is 4.45005'" in {
    process("max 4.45005 -3.75 -17.875") shouldBe "the maximum of 4.45005 -3.75 -17.875 is 4.45005"
  }

  "max 0" should "start like 'Error: '" in {
    assert(process("max 0").startsWith("Error: "))
  }
}