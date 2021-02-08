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
        else Left(ErrorMessage(s"Arguments for command $SUM should be a numbers"))
      case AVERAGE :: tail =>
        if (checkListArguments(tail)) Right(Average(tail.map(_.toDouble)))
        else Left(ErrorMessage(s"Arguments for command $AVERAGE should be a numbers"))
      case MIN :: tail =>
        if (checkListArguments(tail)) Right(Min(tail.map(_.toDouble)))
        else Left(ErrorMessage(s"Arguments for command $MIN should be a numbers"))
      case MAX :: tail =>
        if (checkListArguments(tail)) Right(Max(tail.map(_.toDouble)))
        else Left(ErrorMessage(s"Arguments for command $MAX should be a numbers"))
      case _ => Left(ErrorMessage(
        s"""Wrong command. Use one of this:
           |$DIVIDE first number second number
           |$SUM list of numbers
           |$AVERAGE list of numbers
           |$MIN list of numbers
           |$MAX list of numbers
           |For all command arguments separator is  whitespace""".stripMargin))
    }
    // Implementation hints:
    // You can use String#split, convert to List using .toList, then pattern match on:
    //   case x :: xs => ???

    // Consider how to handle extra whitespace gracefully (without errors).
  }

  def checkDivideArguments(xs: List[String]): Either[ErrorMessage, Command] = {
    import Command._
    if (xs.length != 2 || xs.forall(!_.matches("[+-]?([0-9]*[.])?[0-9]+")))
      Left(ErrorMessage("divide command should contains only two arguments and it's should be a numbers"))
    else
      Right(Divide(xs.head.toDouble, xs.last.toDouble))
  }

  // TODO rename this method
  // TODO extract regex
  def checkListArguments(xs: List[String]): Boolean =
    xs.forall(_.matches("[+-]?([0-9]*[.])?[0-9]+"))


  // should return an error (using `Left` channel) in case of division by zero
  // and other invalid operations
  // TODO remove .toString
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
      case _ => Left(ErrorMessage("Wrong command"))
    }
  }

  // TODO remove duplicate string
  def renderResult(x: Result): String = {
    x match {
      case CalculationResult(result, _ @ Divide(dividend, divisor)) =>
        s"$dividend divided by $divisor is $result"
      case CalculationResult(result, _ @ Sum(numbers)) =>
        s"the sum of ${numbers.mkString("", ", ", "")} is $result"
      case CalculationResult(result, _ @ Average(numbers)) =>
        s"the average of ${numbers.mkString("", ", ", "")} is $result"
      case CalculationResult(result, _ @ Min(numbers)) =>
        s"the min of ${numbers.mkString("", ", ", "")} is $result"
      case CalculationResult(result, _ @ Max(numbers)) =>
        s"the max of ${numbers.mkString("", ", ", "")} is $result"
    }
  }

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
