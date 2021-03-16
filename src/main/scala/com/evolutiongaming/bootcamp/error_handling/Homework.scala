package com.evolutiongaming.bootcamp.error_handling

import cats.data.ValidatedNec
import cats.syntax.all._

import java.time.LocalDateTime

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
    final case object CardHolderNameEmpty extends ValidationError {
      override def toString: String = "Card holder should not be empty"
    }
    final case object CardNumberIsNotNumeric extends ValidationError {
      override def toString: String = "Card number should include only number"
    }
    final case object CardNumberInvalid extends ValidationError {
      override def toString: String = "Card number have size 16 numbers"
    }
    final case object CardExpirationDateFormatInvalid extends ValidationError {
      override def toString: String = "Card expiration should have MM/YYYY format"
    }
    final case object CardExpired extends ValidationError {
      override def toString: String = "Card expired"
    }
    final case object SecurityCodeFormatInvalid extends ValidationError {
      override def toString: String = "Card security code should have 3 or 4 integer number"
    }
    final case object SecurityCodeInvalid extends ValidationError {
      override def toString: String = "Card security code is invalid"
    }
  }

  object PaymentCardValidator {
    import ValidationError._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    private def validateCardHolderName(name: String): AllErrorsOr[CardHolder] = {
      def validateFirstAndLastNameExisting: AllErrorsOr[(String, String)] = {
        val maybeNames = name.split(" ")
        if (maybeNames.length < 2) CardHolderNameInvalid.invalidNec
        else (maybeNames.head, maybeNames.last).validNec
      }

      def validateFirstAndLastNameIsNotEmpty(fullName: (String, String)): AllErrorsOr[(String, String)] = {
        val (first, last) = fullName
        if (first.trim.isEmpty && last.trim.isEmpty) CardHolderNameEmpty.invalidNec
        else (first, last).validNec
      }

      (validateFirstAndLastNameExisting andThen validateFirstAndLastNameIsNotEmpty)
        .map { case (first, last) => CardHolder(first, last) }
    }

    private def validateCardNumber(number: String): AllErrorsOr[CardNumber] = {
      def validateCardNumberLength: AllErrorsOr[String] =
        if (number.length != 16) CardNumberInvalid.invalidNec
        else number.validNec

      def validateCardNumberFormat: AllErrorsOr[String] =
        if (number.forall(_.isDigit)) number.validNec
        else CardNumberIsNotNumeric.invalidNec

      validateCardNumberLength.productR(validateCardNumberFormat).map(CardNumber)
    }

    private def validateExpirationDate(
      expirationDate: String,
      separator: String = "/"
    ): AllErrorsOr[CardExpirationDate] = {
      def validateExpirationDateFormat: AllErrorsOr[(String, String)] = {
        val maybeExpireDate = expirationDate.split(separator)
        if (maybeExpireDate.size == 2) (maybeExpireDate.head, maybeExpireDate.last).validNec
        else CardExpirationDateFormatInvalid.invalidNec
      }

      def validateExpirationDateContent(expirationDate: (String, String)): AllErrorsOr[(String, String)] = {
        val (expirationMonth, expirationYear) = expirationDate
        if (isNonEmptyNumericMonth(expirationMonth) && isNonEmptyNumericYear(expirationYear))
          expirationDate.validNec
        else
          CardExpirationDateFormatInvalid.invalidNec
      }

      def validateExpiration(expirationDate: (String, String)): AllErrorsOr[(String, String)] = {
        val (expirationMonth, expirationYear) = expirationDate
        val expMonth = expirationMonth.toInt
        val expYear = expirationYear.toInt
        val now = LocalDateTime.now()
        if (now.getMonthValue < expMonth && now.getYear < expYear) CardExpired.invalidNec
        else expirationDate.validNec
      }

      def isNonEmptyNumericMonth(s: String): Boolean =
        isNonEmptyNumericWithLength(s, 2)

      def isNonEmptyNumericYear(s: String): Boolean =
        isNonEmptyNumericWithLength(s, 4)

      def isNonEmptyNumericWithLength(s: String, length: Int): Boolean =
        s.trim.nonEmpty && s.trim.length == length && s.trim.forall(_.isDigit)

      (validateExpirationDateFormat
        andThen validateExpirationDateContent
        andThen validateExpiration).map { case (month, year) => CardExpirationDate(month, year) }
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