package com.evolutiongaming.bootcamp.basics

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ControlStructureSpec extends AnyWordSpec with Matchers {

  import ControlStructure._
  import ControlStructure.Command._

  "parseCommand" should {
    "correct parse divide command" in {
      parseCommand("divide 10 2") shouldBe Right(Divide(10.0, 2.0))
      parseCommand("divide 12.8 2.4") shouldBe Right(Divide(12.8, 2.4))
    }

    "correct parse sum command" in {
      parseCommand("sum 1 2 3 4 5") shouldBe Right(Sum(List(1, 2, 3, 4, 5)))
    }

    "correct parse average command" in {
      parseCommand("average 2.2 7 4.5 9 13.7") shouldBe Right(Average(List(2.2, 7, 4.5, 9, 13.7)))
    }

    "correct parse min command" in {
      parseCommand("min -2.2 7 5 9 -13.7") shouldBe Right(Min(List(-2.2, 7, 5, 9, -13.7)))
    }

    "correct parse max command" in {
      parseCommand("max 7 2.1 13.7 1 73") shouldBe Right(Max(List(7, 2.1, 13.7, 1, 73)))
    }

    "return correct error message" in {
      val divideErrorMessage =
        Left(ErrorMessage("divide command should contains only two arguments and it's should be a numbers"))
      val otherCommandErrorMessage =
        Left(ErrorMessage("Arguments for command sum should be a numbers"))

      parseCommand("divide 10 2 10 123") shouldBe divideErrorMessage
      parseCommand("divide asddaw") shouldBe divideErrorMessage
      parseCommand("sum a c d e f") shouldBe otherCommandErrorMessage
    }

    "handle extra whitespace" in {
      parseCommand("divide 10       2") shouldBe Right(Divide(10.0, 2.0))
    }
  }

  "calculate" should {
    "correct calculating divide command" in {
      calculate(Divide(12.8, 2.4)) shouldBe
        Right(CalculationResult(5.333333333333334, Divide(12.8, 2.4)))
    }

    "correct calculating sum command" in {
      calculate(Sum(List(1, 2, 3, 4, 5))) shouldBe
        Right(CalculationResult(15.0, Sum(List(1, 2, 3, 4, 5))))
    }

    "correct calculating average command" in {
      calculate(Average(List(2.1, 7, 4.5, 9, 13))) shouldBe
        Right(CalculationResult(7.12, Average(List(2.1, 7, 4.5, 9, 13))))
    }

    "correct calculating min command" in {
      calculate(Min(List(-2.2, 7, 5, 9, -13.7))) shouldBe
        Right(CalculationResult(-13.7, Min(List(-2.2, 7, 5, 9, -13.7))))
    }

    "correct calculating max command" in {
      calculate(Max(List(7, 2.1, 13.7, 1, 73))) shouldBe
        Right(CalculationResult(73.0, Max(List(7, 2.1, 13.7, 1, 73))))
    }

    "correct handle dived by zero" in {
      calculate(Divide(12.8, 0)) shouldBe Left(ErrorMessage("Divided by zero"))
    }
  }
}
