package error_handling

import java.time.{Month, YearMonth}

import cats.data.ValidatedNec
import cats.implicits.{catsSyntaxApply, catsSyntaxTuple4Semigroupal, catsSyntaxValidatedIdBinCompat0}
import error_handling.ErrorHandling.NumberType.{LONG, MEDIUM, SHORT}
import error_handling.ErrorHandling.ValidationError._

import scala.annotation.tailrec
import scala.util.matching.Regex

object ErrorHandling {
  trait NumberType {
    def value: Byte
  }

  object NumberType {
    case object SHORT extends NumberType {
      override def value: Byte = 13
    }

    case object MEDIUM extends NumberType {
      override def value: Byte = 16
    }

    case object LONG extends NumberType {
      override def value: Byte = 19
    }
  }

  final case class CardNumber private(number: Long, numberType: NumberType)
  object CardNumber {
    def from(number: Long, numberType: NumberType): Option[CardNumber] = numberType match {
      case SHORT => if (number <= 9999999999999L) Option.apply(CardNumber(number, SHORT)) else Option.empty
      case MEDIUM => if (number <= 9999999999999999L) Option.apply(CardNumber(number, MEDIUM)) else Option.empty
      case LONG => Option.apply(CardNumber(number, LONG))
    }
  }

  /**
   * Class represents a general bank credit card instance
   *
   * @param name           full name of card holder. Name and surname divided by gap with possible hyphens in upper case
   *                       full name length is 2..26 according to 'ISO IEC 7813'
   * @param number         credit card number wrapped in sum adt. Consists of 13, 16 or 19 digits
   * @param expirationDate credit card expiration date
   * @param securityCode   security code - CVV or CVC. Consists of 3 digits
   */
  case class CreditCard protected(name: String, number: CardNumber, expirationDate: YearMonth, securityCode: Short)

  sealed trait ValidationError {
    def message: String
  }
  object ValidationError {
    case object NameLengthError extends ValidationError {
      override def message: String = "Name length should be in range 2..26"
    }
    case object NumberLengthError extends ValidationError {
      override def message: String = "Number should contain 13, 16 or 19 digits"
    }
    case object DataInputError extends ValidationError {
      override def message: String = "Cannot parse date from given value"
    }
    case object SecurityCodeLengthError extends ValidationError {
      override def message: String = "Security code should have length 3"
    }
    case object NameFormatError extends ValidationError {
      override def message: String = "Name should contain name and surname separated by gap with possible hyphens"
    }
    case object NumberFormatError extends ValidationError {
      override def message: String = "Specific number should have 16 length format"
    }
    case object PaymentSystemError extends ValidationError {
      override def message: String = "Not appropriate payment system."
    }
    case object DateExpirationError extends ValidationError {
      override def message: String = "Card is expired"
    }
    case object BusinessMonthError extends ValidationError {
      override def message: String = "Card should be expired on business month."
    }
    case object SecurityCodeWeakError extends ValidationError {
      override def message: String = "Security code is weak."
    }
    case object BasicCardError extends ValidationError {
      override def message: String = "Cannot create instance of credit card."
    }
  }

  object CreditCardValidator {

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    /**
     * Create a credit card of specific type
     *
     * @param name           should correspond {@link CreditCard} requirements
     * @param number         should have 'American express' payment system(starts from '3') and {@link MEDIUM} format
     * @param expirationDate should be 'future' date and end on March
     * @param securityCode   should consists at less from 2 different digits (more strict)
     */
    def validate(name: String, number: CardNumber, expirationDate: YearMonth, securityCode: Short): AllErrorsOr[CreditCard] =
      (validateNameLength(name).andThen(validateName),
        validateNumberLength(number).andThen(validateNumber),
        validateDate(expirationDate),
        validateSecurityCodeLength(securityCode).andThen(validateSecurityCode))
        .mapN((name, number, expirationDate, securityCode) => CreditCard(name, number, expirationDate, securityCode))

    //NAME VALIDATORS
    private def validateNameLength(name: String): AllErrorsOr[String] =
      if (2 to 26 contains name.length) name.validNec else NameLengthError.invalidNec

    private def validateName(name: String): AllErrorsOr[String] =
      if (matches(name, "^[A-Z][A-Z\\-]+[\\s]{1}[A-Z\\-]+[A-Z]$".r)) name.validNec else NameFormatError.invalidNec

    //NUMBER VALIDATORS
    private def validateNumberLength(number: CardNumber): AllErrorsOr[CardNumber] = {
      val numberString: String = increaseToLength(number.number.toString, number.numberType.value)
      if (13 to 19 by 3 contains numberString.length) number.validNec else NumberLengthError.invalidNec
    }

    private def validateNumber(number: CardNumber): AllErrorsOr[CardNumber] = {
      validateNumberFormat(number) productR validatePaymentSystem(number)
    }

    private def validateNumberFormat(number: CardNumber): AllErrorsOr[CardNumber] = {
      val numberString: String = increaseToLength(number.number.toString, number.numberType.value)
      if (numberString.length == 16) number.validNec else NumberFormatError.invalidNec
    }

    private def validatePaymentSystem(number: CardNumber): AllErrorsOr[CardNumber] =
      if (number.number.toString.startsWith("3")) number.validNec else PaymentSystemError.invalidNec

    private def validateDate(expirationDate: YearMonth): AllErrorsOr[YearMonth] = {
      validateDateExpired(expirationDate) productR validateBusinessMonth(expirationDate)
    }

    private def validateDateExpired(expirationDate: YearMonth): AllErrorsOr[YearMonth] = {
      if (expirationDate.isAfter(YearMonth.now()))
        expirationDate.validNec else DateExpirationError.invalidNec
    }

    private def validateBusinessMonth(expirationDate: YearMonth): AllErrorsOr[YearMonth] =
      if (expirationDate.getMonth == Month.MARCH) expirationDate.validNec else BusinessMonthError.invalidNec

    //CODE VALIDATORS
    private def validateSecurityCodeLength(securityCode: Short): AllErrorsOr[Short] =
      if (1 to 999 contains securityCode)
        securityCode.validNec else SecurityCodeLengthError.invalidNec

    private def validateSecurityCode(securityCode: Short): AllErrorsOr[Short] = {
      if (matches(increaseToLength(securityCode.toString, 3), "(.)\\1*".r))
        SecurityCodeWeakError.invalidNec else securityCode.validNec
    }

    @tailrec
    private def increaseToLength(number: String, length: Int): String =
      if (number.length < length) increaseToLength(s"0$number", length) else number

    protected val matches = (value: String, regex: Regex) => value match {
      case regex(_*) => true
      case _ => false
    }
  }
}