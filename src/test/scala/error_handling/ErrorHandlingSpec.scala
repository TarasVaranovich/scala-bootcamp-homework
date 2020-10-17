package error_handling

import error_handling.ErrorHandling.CreditCardValidator.AllErrorsOr
import error_handling.ErrorHandling.{CreditCard, CreditCardValidator, ValidationError}
import org.scalatest.freespec.AnyFreeSpec

class ErrorHandlingSpec extends AnyFreeSpec {

  "Credit card" - {
    "successfully created" in {
      val creditCard = CreditCard.from("JOHN-OLIVER SMITH-JUNIOR", "1234567812345678",
        "05/23", "012")
      assert(creditCard.isDefined)
    }
    "not created with to length name" in {
      val creditCard = CreditCard.from("JOHN-OLIVER WILLIAMS-JUNIOR", "1234567812345678",
        "05/23", "012")
      assert(creditCard.isEmpty)
    }

    "not created with to length number" in {
      val creditCard = CreditCard.from("JOHN-OLIVER SMITH-JUNIOR", "12345678123456780011",
        "05/23", "012")
      assert(creditCard.isEmpty)
    }

    "not created with wrong expiration date format" in {
      val creditCard = CreditCard.from("JOHN-OLIVER SMITH-JUNIOR", "1234567812345678",
        "5/2021", "012")
      assert(creditCard.isEmpty)
    }

    "not created with to short security code" in {
      val creditCard = CreditCard.from("JOHN-OLIVER SMITH-JUNIOR", "1234567812345678",
        "05/23", "12")
      assert(creditCard.isEmpty)
    }
  }

  "CreditCardValidator" - {
    "successfully creates card" in {
      val result: AllErrorsOr[CreditCard] =
        CreditCardValidator.validate("JOHN SMITH", "3234567812345678", "03/24", "123")
      assert(result.isValid)
    }

    "collects errors due creation of card with invalid data" in {
      val result: AllErrorsOr[CreditCard] =
        CreditCardValidator.validate("J-OHN SMIT-H", "4234567812345678000", "04/24", "444")
      val errors: List[ValidationError] = result.fold(chain => chain.toNonEmptyList.toList, _ => List.empty)
      assert(errors.size == 5)
    }

    "collects errors due creation of card with empty data" in {
      val result: AllErrorsOr[CreditCard] =
        CreditCardValidator.validate(null, null, null, null)
      val errors: List[ValidationError] = result.fold(chain => chain.toNonEmptyList.toList, _ => List.empty)
      assert(errors.size == 4)
    }
  }
}