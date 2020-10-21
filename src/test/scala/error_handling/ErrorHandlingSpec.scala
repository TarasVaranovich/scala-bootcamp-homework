package error_handling

import java.time.{Month, YearMonth}

import error_handling.ErrorHandling.CreditCardValidator.AllErrorsOr
import error_handling.ErrorHandling.NumberType.{LONG, MEDIUM, SHORT}
import error_handling.ErrorHandling._
import org.scalatest.freespec.AnyFreeSpec

class ErrorHandlingSpec extends AnyFreeSpec {
  "Card number" - {
    "successfully created with short type" in {
      assert(CardNumber.from(1234567890123L, SHORT).isDefined)
    }

    "successfully created with medium type" in {
      assert(CardNumber.from(1234567890123999L, MEDIUM).isDefined)
    }

    "successfully created with long type" in {
      assert(CardNumber.from(1234567890123999999L, LONG).isDefined)
    }

    "not created for short type with medium argument" in {
      assert(CardNumber.from(1234567890123999L, SHORT).isEmpty)
    }

    "not created for medium type with long argument" in {
      assert(CardNumber.from(1234567890123999999L, MEDIUM).isEmpty)
    }
  }

  "Credit card" - {
    "successfully created" in {
      val creditCard = CreditCard("JOHN-OLIVER SMITH-JUNIOR",
        CardNumber.from(1234567812345678L, NumberType.MEDIUM).get,
        YearMonth.of(2023, Month.MARCH),
        12)
      assert(creditCard.name.equals("JOHN-OLIVER SMITH-JUNIOR"))
      assert(creditCard.number.number.toString.equals("1234567812345678"))
      assert(creditCard.expirationDate.getMonth.getValue == 3)
      assert(creditCard.expirationDate.getYear == 2023)
      assert(creditCard.securityCode == 12)
    }
  }

  "CreditCardValidator" - {
    "successfully creates card" in {
      val result: AllErrorsOr[CreditCard] = CreditCardValidator.validate(
        "JOHN-OLIVER SMITH-JUNIOR",
        CardNumber.from(3234567812345678L, MEDIUM).get,
        YearMonth.of(2024, Month.MARCH), 23)
      assert(result.isValid)
    }

    "collects errors due creation of card with invalid data" in {
      val result: AllErrorsOr[CreditCard] = CreditCardValidator.validate(
        "JS",
        CardNumber.from(4234567812345678L, MEDIUM).get,
        YearMonth.of(2018, Month.APRIL), 0)
      val errors: List[ValidationError] = result.fold(chain => chain.toNonEmptyList.toList, _ => List.empty)
      assert(errors.size == 5)
    }
  }
}