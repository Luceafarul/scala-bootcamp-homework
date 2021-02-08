package com.evolutiongaming.bootcamp.basics

import scala.io.Source
import com.evolutiongaming.bootcamp.basics.ControlStructure.Command._

object ControlStructure {

  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command

    val DIVIDE = "divide"
    val SUM = "sum"
    val AVERAGE = "average"
    val MIN = "min"
    val MAX = "max"
  }

  final case class ErrorMessage(value: String)

  // Adjust `Result` and `ChangeMe` as you wish - you can turn Result into a `case class`
  // and remove the `ChangeMe` if you think it is the best model for your solution,
  // or just have other `case class`-es implement `Result`
  sealed trait Result
  final case class CalculationResult(result: Double, command: Command) extends Result

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    x.toLowerCase.split(" ").filterNot(_.isBlank).toList match {
      case DIVIDE :: tail =>
        checkDivideArguments(tail)
      case SUM :: tail =>
        if (checkListArguments(tail)) Right(Sum(tail.map(_.toDouble)))
        else Left(generalErrorMessage(SUM))
      case AVERAGE :: tail =>
        if (checkListArguments(tail)) Right(Average(tail.map(_.toDouble)))
        else Left(generalErrorMessage(AVERAGE))
      case MIN :: tail =>
        if (checkListArguments(tail)) Right(Min(tail.map(_.toDouble)))
        else Left(generalErrorMessage(MIN))
      case MAX :: tail =>
        if (checkListArguments(tail)) Right(Max(tail.map(_.toDouble)))
        else Left(generalErrorMessage(MAX))
      case _ => Left(ErrorMessage(
        s"""Wrong command. Use one of this:
           |- $DIVIDE first number second number
           |- $SUM list of numbers
           |- $AVERAGE list of numbers
           |- $MIN list of numbers
           |- $MAX list of numbers
           |For all command arguments separator is  whitespace""".stripMargin))
    }
  }

  private def checkDivideArguments(xs: List[String]): Either[ErrorMessage, Command] = {
    import Command._
    if (xs.length != 2 || xs.forall(!_.matches("[+-]?([0-9]*[.])?[0-9]+")))
      Left(ErrorMessage("divide command should contains only two arguments and it's should be a numbers"))
    else
      Right(Divide(xs.head.toDouble, xs.last.toDouble))
  }

  private def checkListArguments(xs: List[String]): Boolean = {
    val onlyNumbers = "[+-]?([0-9]*[.])?[0-9]+"
    xs.forall(_.matches(onlyNumbers))
  }

  private def generalErrorMessage(command: String): ErrorMessage =
    ErrorMessage(s"Arguments for command $command should be a numbers")

  // should return an error (using `Left` channel) in case of division by zero
  // and other invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Divide(_, 0) => Left(ErrorMessage("Divided by zero"))
      case divide @ Divide(dividend, divisor) =>
        Right(CalculationResult(dividend / divisor, divide))
      case sum @ Sum(numbers) =>
        Right(CalculationResult(numbers.sum, sum))
      case average @ Average(numbers) =>
        Right(CalculationResult(numbers.sum / numbers.length, average))
      case min @ Min(numbers) =>
        Right(CalculationResult(numbers.min, min))
      case max @ Max(numbers) =>
        Right(CalculationResult(numbers.max, max))
      case other => Left(ErrorMessage(s"$other command does not exist"))
    }
  }

  def renderResult(x: Result): String = {
    x match {
      case CalculationResult(result, _ @ Divide(dividend, divisor)) =>
        s"$dividend divided by $divisor is $result"
      case CalculationResult(result, _ @ Sum(numbers)) =>
        generalRenderResult(numbers, result, SUM)
      case CalculationResult(result, _ @ Average(numbers)) =>
        generalRenderResult(numbers, result, AVERAGE)
      case CalculationResult(result, _ @ Min(numbers)) =>
        generalRenderResult(numbers, result, MIN)
      case CalculationResult(result, _ @ Max(numbers)) =>
        generalRenderResult(numbers, result, MAX)
    }
  }

  private def generalRenderResult(xs: List[Double], result: Double, command: String): String =
    s"the $command of ${xs.mkString("", " ", "")} is $result"

  def process(x: String): String = {
    import cats.implicits._
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.

    val result = for {
      command <- parseCommand(x).leftMap(_.value)
      result <- calculate(command).leftMap(_.value)
    } yield renderResult(result)

    result.merge
  }

  // This `main` method reads lines from stdin,
  // passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit =
    Source.stdin.getLines() map process foreach println
}
