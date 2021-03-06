package containers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.matchers.should.Matchers.not.be
import org.scalatest.verbs.ShouldVerb

import scala.language.postfixOps

class MailAgentSpec extends AnyFlatSpec with ShouldVerb {
  "MailAgent" should "send example email in configured environment" in {
    val ma = new MailAgent()
    val result: Either[Throwable, Unit] = ma.sendExampleEmail("Test Example", "Example body.")
    result should be (Right)
  }
}