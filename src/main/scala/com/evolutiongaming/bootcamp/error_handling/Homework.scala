package com.evolutiongaming.bootcamp.error_handling

import cats.data.ValidatedNec
import cats.syntax.all._
import com.evolutiongaming.bootcamp.error_handling.Homework.ValidationError.{CardHolderNameInvalid, SecurityCodeFormatInvalid, SecurityCodeInvalid}

// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
object Homework {

  final case class CardHolder(firstName: String, lastName: String)
  final case class CardNumber(number: String)
  final case class CardExpirationDate(month: String, year: String)
  final case class CardSecurityCode(securityCode: String)
  case class PaymentCard(
    holder: CardHolder,
    number: CardNumber,
    expirationDate: CardExpirationDate,
    securityCode: CardSecurityCode
  )

  sealed trait ValidationError
  object ValidationError {
    // Add errors as needed
    final case object CardHolderNameInvalid extends ValidationError {
      override def toString: String =
        "Card holder name should include first name and last name separated by space"
    }
    final case object CardNumberIsNotNumeric extends ValidationError {
      override def toString: String = "Card number should include only number"
    }
    final case object CardNumberInvalid extends ValidationError {

    }
    final case object CardExpirationDateFormatInvalid extends ValidationError
    final case object CardExpired extends ValidationError
    final case object SecurityCodeFormatInvalid extends ValidationError
    final case object SecurityCodeInvalid extends ValidationError
  }

  object PaymentCardValidator {

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    private def validateCardHolderName(name: String): AllErrorsOr[CardHolder] = {
      def validateFirstAndLastNameExisting: AllErrorsOr[(String, String)] = {
        val maybeNames = name.split(" ")
        if (maybeNames.length < 2) CardHolderNameInvalid.invalidNec
        else (maybeNames.head, maybeNames.last).validNec
      }

      ???
    }

    private def validateCardNumber(number: String): AllErrorsOr[CardNumber] = ???

    private def validateExpirationDate(
      expirationDate: String,
      separator: String = "/"
    ): AllErrorsOr[CardExpirationDate] = {
      def validateExpirationDateLength: AllErrorsOr[String] = ???
      def validateExpirationDateFormat: AllErrorsOr[String] = ???
      def validateExpirationDateContent: AllErrorsOr[String] = ???
      ???
    }

    private def validateSecurityCode(securityCode: String): AllErrorsOr[CardSecurityCode] = {
      val CVV2_LENGTH_3 = 3
      val CVV2_LENGTH_4 = 4

      def validateSecurityCodeLength: AllErrorsOr[String] = {
        val length = securityCode.length
        if (length == CVV2_LENGTH_3 || length == CVV2_LENGTH_4) securityCode.validNec
        else SecurityCodeFormatInvalid.invalidNec
      }

      def validateSecurityCodeContent(code: String): AllErrorsOr[String] =
        if (code.forall(_.isDigit)) code.validNec
        else SecurityCodeInvalid.invalidNec

      // TODO check that error was composed
      (validateSecurityCodeLength andThen validateSecurityCodeContent).map(CardSecurityCode)
    }

    def validate(
      name: String,
      number: String,
      expirationDate: String,
      securityCode: String,
    ): AllErrorsOr[PaymentCard] = {
      (
        validateCardHolderName(name),
        validateCardNumber(number),
        validateExpirationDate(expirationDate),
        validateSecurityCode(securityCode)
      ).mapN(PaymentCard)
    }
  }
}