package error_handling

import java.util.Calendar

import error_handling.ErrorHandling.CreditCard
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.matching.Regex

class RegexpTest extends AnyFlatSpec {
  "name regex" should "block invalid names and pass valid names" in {
    val nameRegex: Regex = "^[A-Z][A-Z\\-]+[\\s]{1}[A-Z\\-]+[A-Z]$".r
    val nameLengthRegex: Regex = "[A-Z\\-\\s]{2,26}".r

    assert(regexMatch("JOHN-OLIVER SMITH-JUNIOR", nameRegex))
    assert(!regexMatch("JOHN-OLIVERSMITH-JUNIOR", nameRegex))
    assert(!regexMatch(" JOHN-OLIVERSMITH-JUNIOR", nameRegex))
    assert(!regexMatch("-OLIVER SMITH-JUNIOR", nameRegex))
    assert(!regexMatch("JOHN-OLIVER SMITH-", nameRegex))
    assert(!regexMatch("J", nameRegex))
    assert(!regexMatch("JOHN-OLIVER WILLIAMS-JUNIOR", nameLengthRegex))
  }

  "number regex" should "block invalid numbers and pass valid numbers" in {
    val numberRegex: Regex = "[0-9]{13}|[0-9]{16}|[0-9]{19}".r
    assert(regexMatch("1234567812345", numberRegex))
    assert(regexMatch("1234567812345678", numberRegex))
    assert(regexMatch("1234567812345678000", numberRegex))
    assert(!regexMatch("12345678123456780", numberRegex))
    assert(regexMatch("3234567812345678", numberRegex))
  }

  "expiration date regex" should "block invalid dates and pass valid dates" in {
    val expirationDateRegex: Regex = "[0-9]{2}[/]{1}[0-9]{2}".r
    assert(regexMatch("05/23", expirationDateRegex))
    assert(!regexMatch("05 23", expirationDateRegex))
    assert(!regexMatch("0523", expirationDateRegex))
    assert(!regexMatch("/0523", expirationDateRegex))
    assert(!regexMatch("105/239", expirationDateRegex))
  }

  "security code regex" should "block invalid codes and pass valid codes" in {
    val securityCodeRegex: Regex = "[0-9]{3}".r
    assert(regexMatch("011", securityCodeRegex))
    assert(!regexMatch("1211", securityCodeRegex))
    assert(!regexMatch("7", securityCodeRegex))
    assert(!regexMatch("abc", securityCodeRegex))
  }

  "all characters equal regex" should "block string with different characters and pass with equal" in {
    val equalCharactersRegex: String = "(.)\\1*"
    assert("222".matches(equalCharactersRegex))
    assert(!"922".matches(equalCharactersRegex))
  }

  "expiration date" should "be after current date" in {
    val dateString: String = "03/23"
    val month: Int = dateString.split("/").head.toInt
    val year: Int = dateString.split("/").last.toInt + 2000
    import java.util.GregorianCalendar
    val expirationDate: Calendar = new GregorianCalendar(year, month - 1, 1)
    val currentDate: Calendar = new GregorianCalendar()
    assert(expirationDate.after(currentDate))
  }

  private def regexMatch(value: String, regex: Regex): Boolean = value match {
    case regex(_*) => true
    case _ => false
  }
}