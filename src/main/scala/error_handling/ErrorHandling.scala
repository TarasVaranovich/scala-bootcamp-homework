package error_handling

import java.util.Calendar

import cats.data.ValidatedNec
import cats.implicits.{catsSyntaxApply, catsSyntaxTuple4Semigroupal, catsSyntaxValidatedIdBinCompat0}
import error_handling.ErrorHandling.ValidationError._

import scala.util.matching.Regex

object ErrorHandling {

  /**
   * Class represents a general bank credit card instance
   *
   * @param name           full name of card holder. Name and surname divided by gap with possible hyphens in upper case
   *                       full name length is 2..26 according to 'ISO IEC 7813'
   * @param number         credit card number. Consists of 13, 16 or 19 digits
   * @param expirationDate credit card expiration date
   * @param securityCode   security code - CVV or CVC. Consists of 3 digits
   */
  case class CreditCard protected(name: String, number: String, expirationDate: String, securityCode: String)
  object CreditCard {
    protected val matches = (value: String, regex: Regex) => value match {
      case regex(_*) => true
      case _ => false
    }

    def from(name: String,
             number: String,
             expirationDate: String,
             securityCode: String): Option[CreditCard] = {
      if (matches(name, "^[A-Z][A-Z\\-]+[\\s]{1}[A-Z\\-]+[A-Z]$".r) &&
        matches(name, "[A-Z\\-\\s]{2,26}".r) &&
        matches(number, "[0-9]{13}|[0-9]{16}|[0-9]{19}".r) &&
        matches(expirationDate, "[0-9]{2}[/]{1}[0-9]{2}".r) &&
        matches(securityCode, "[0-9]{3}".r))
        Some(CreditCard(name, number, expirationDate, securityCode)) else None
    }
  }

  sealed trait ValidationError {
    def message: String
  }
  object ValidationError {
    case object NameInputError extends ValidationError {
      override def message: String = "Cannot parse name from given sting"
    }
    case object NumberInputError extends ValidationError {
      override def message: String = "Cannot parse number from given string"
    }
    case object DataInputError extends ValidationError {
      override def message: String = "Cannot parse date from given value"
    }
    case object CodeInputError extends ValidationError {
      override def message: String = "Cannot parse security code from given value"
    }
    case object SimpleNameConventionError extends ValidationError {
      override def message: String = "Simple name convention requires absence of hyphens."
    }
    case object CardNumberFormatError extends ValidationError {
      override def message: String = "Not appropriate card number length."
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
     * @param name           should be s simple version of name without hyphens
     * @param number         should be only 16 digit number with 'American express' payment system(starts from '3')
     * @param expirationDate should be 'future' date and end on March
     * @param securityCode   should consists at less from 2 different digits (more strict)
     */
    def validate(name: String, number: String, expirationDate: String, securityCode: String): AllErrorsOr[CreditCard] = {
      (validateNameInput(name).andThen(validateName),
        validateNumberInput(number).andThen(validateNumber),
        validateDateInput(expirationDate).andThen(validateDate),
        validateSecurityCodeInput(securityCode).andThen(validateSecurityCode))
        .mapN((name, number, expirationDate, securityCode) =>
          CreditCard.from(name, number, expirationDate, securityCode))
        .andThen {
          case Some(card) => card.validNec
          case None => BasicCardError.invalidNec
        }
    }

    //NAME VALIDATORS
    private def validateNameInput(name: String): AllErrorsOr[String] =
      if (Option(name).isDefined && name.length >= 3) name.validNec else NameInputError.invalidNec

    private def validateName(name: String): AllErrorsOr[String] =
      if (name.contains("-")) SimpleNameConventionError.invalidNec else name.validNec

    //NUMBER VALIDATORS
    private def validateNumberInput(number: String): AllErrorsOr[String] =
      if (Option(number).isDefined && number.matches("[0-9]{13,19}"))
        number.validNec else NumberInputError.invalidNec

    private def validateNumber(number: String): AllErrorsOr[String] = {
      validateNumberLength(number) productR validatePaymentSystem(number)
    }
    private def validateNumberLength(number: String): AllErrorsOr[String] =
      if (number.length == 16) number.validNec else CardNumberFormatError.invalidNec

    private def validatePaymentSystem(number: String): AllErrorsOr[String] =
      if (number.startsWith("3")) number.validNec else PaymentSystemError.invalidNec

    //DATE VALIDATORS
    private def validateDateInput(date: String): AllErrorsOr[String] = {
      val checkDateString = (dateStr: String) => {
        val dateStrings: Array[String] = dateStr.split("/")
        val dateValue: Array[String] = dateStrings.filter(_.matches("[0-9]{2}"))
        dateValue.length == 2
      }
      if (Option(date).isDefined && date.length == 5 && checkDateString(date))
        date.validNec else DataInputError.invalidNec
    }

    private def validateDate(expirationDate: String): AllErrorsOr[String] = {
      validateDateExpired(expirationDate) productR validateBusinessMonth(expirationDate)
    }

    private def validateDateExpired(expirationDate: String): AllErrorsOr[String] = {
      import java.util.GregorianCalendar
      val month: Int = expirationDate.split("/").head.toInt
      val year: Int = expirationDate.split("/").last.toInt + 2000
      val expirationCalendar: Calendar = new GregorianCalendar(year, month - 1, 1)
      val currentCalendar: Calendar = new GregorianCalendar()
      if (expirationCalendar.after(currentCalendar)) expirationDate.validNec else DateExpirationError.invalidNec
    }

    private def validateBusinessMonth(expirationDate: String): AllErrorsOr[String] =
      if (expirationDate.startsWith("03")) expirationDate.validNec else BusinessMonthError.invalidNec

    //CODE VALIDATORS
    private def validateSecurityCodeInput(securityCode: String): AllErrorsOr[String] =
      if (Option(securityCode).isDefined && securityCode.matches("[0-9]{3}"))
        securityCode.validNec else CodeInputError.invalidNec

    private def validateSecurityCode(securityCode: String): AllErrorsOr[String] =
      if (securityCode.matches("(.)\\1*")) SecurityCodeWeakError.invalidNec else securityCode.validNec
  }
}