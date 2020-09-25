import basics.Basics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class BasicsSpec extends AnyFlatSpec {

  "LCD of 12 and 18" should "be 36" in {
    Basics.lcm(12, 18) shouldBe 36
  }

  "LCD of 10,025,005 and 9,900,044" should "throw exception" in {
    assertThrows[Exception](Basics.lcm(10025005, 9900044))
  }

  "LCD of 69941 and 13679" should "be 956722939" in {
    Basics.lcm(69941, 13679) shouldBe 956722939
  }

  "LCD of 0 and 0" should "throw exception" in {
    assertThrows[Exception](Basics.lcm(0, 0))
  }

  "GCD of 54 and 24" should "be 6" in {
    Basics.gcd(54, 24) shouldBe 6
  }
}
