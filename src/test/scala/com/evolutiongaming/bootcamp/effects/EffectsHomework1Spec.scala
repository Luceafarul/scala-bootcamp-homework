package com.evolutiongaming.bootcamp.effects

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.Future

class EffectsHomework1Spec extends AsyncWordSpec with Matchers {

  import EffectsHomework1._

  "IO" should {
    "create new IO" in {
      val io = IO { "This is string" }

      io shouldBe a[IO[String]]
      io should not be "This is string"
    }

    "unsafeRunSync should produce IO value" in {
      val io = IO("This is string")
      io.unsafeRunSync() shouldBe "This is string"
    }

    "unsafeToFuture should produce IO value and wrap it into future" in {
      val io = IO("This is string")
      io.unsafeToFuture() map { result => result shouldBe "This is string" }
    }

    "map should apply function to IO value" in {
      val ioString = IO { "10" }
      val ioInt = ioString.map(_.toInt)

      ioInt shouldBe a[IO[Int]]
      ioInt.unsafeRunSync() shouldBe 10
    }

    "flatMap should apply function to IO value" in {
      val ioString = IO { "10" }
      val ioInt = ioString.flatMap(n => IO(n.toInt))

      ioInt shouldBe a[IO[Int]]
      ioInt.unsafeRunSync() shouldBe 10
    }

    "*> should return second IO" in {
      val ioString = IO { "10" }
      val ioInt = IO { 73 }

      val result = ioString *> ioInt

      result shouldBe a[IO[Int]]
      result.unsafeRunSync() shouldBe 73
    }

    "as should wrap new value to IO" in {
      val ioString = IO { "10" }
      val ioInt = ioString.as(73)

      ioInt shouldBe a[IO[Int]]
      ioInt.unsafeRunSync() shouldBe 73
    }

    "void should convert IO[A] to IO[Unit]" in {
      val ioString = IO { "This is string" }
      val ioUnit = ioString.void

      ioUnit shouldBe a[IO[Unit]]
      ioUnit.unsafeRunSync() shouldBe ()
    }

    "attempt should wrap execution into Either, and return handle exception if it occur" in {
      val ioString = IO { "This is string" }
      val ioWithException = ioString.map(_.toInt).attempt

      ioWithException shouldBe a[IO[Either[Throwable, Int]]]
      ioWithException.unsafeRunSync().isLeft shouldBe true
      // ioWithException.unsafeRunSync() should be (Try("This is string".toInt).toEither.leftSideValue)
      // TODO: how to validate left side?
    }

    "attempt should wrap execution into Either, and return value if exception not raised" in {
      val ioString = IO { "10" }
      val ioInt = ioString.map(_.toInt).attempt

      ioInt shouldBe a[IO[Either[Throwable, Int]]]
      ioInt.unsafeRunSync().isRight shouldBe true
      ioInt.unsafeRunSync() shouldBe Right(10)
    }

    "option should replace failure with an empty Option" in {
      val ioString = IO { "This is string" }
      val ioWithException = ioString.map(_.toInt).option

      ioWithException shouldBe a[IO[Option[Int]]]
      ioWithException.unsafeRunSync() shouldBe None
    }

    "option should wrap value with Some" in {
      val ioString = IO { "10" }
      val ioInt = ioString.map(_.toInt).option

      ioInt shouldBe a[IO[Option[Int]]]
      ioInt.unsafeRunSync() shouldBe Some(10)
    }

    "handleErrorWith should recover after exception" in {
      val defaultValue = 0
      val ioString = IO { "This is string" }
      val ioWithException = ioString.map(_.toInt).handleErrorWith(_ => IO { defaultValue })

      ioWithException shouldBe a[IO[Int]]
      ioWithException.unsafeRunSync() shouldBe defaultValue
    }

    "redeem should recover after exception with recover function" in {
      val defaultValue = 0
      val ioString = IO { "This is string" }
      val ioWithException = ioString.map(_.toInt).redeem(_ => defaultValue, x => x * x)

      ioWithException shouldBe a[IO[Int]]
      ioWithException.unsafeRunSync() shouldBe defaultValue
    }

    "redeem should transform result of source on evaluation if exception not occur" in {
      val defaultValue = 0
      val ioString = IO { "10" }
      val ioWithException = ioString.map(_.toInt).redeem(_ => IO { defaultValue }, x => x * x)

      ioWithException shouldBe a[IO[Int]]
      ioWithException.unsafeRunSync() shouldBe 100
    }

    "redeemWith should recover after exception with recover function" in {
      val defaultValue = 0
      val ioString = IO { "This is string" }
      val ioWithException = ioString.map(_.toInt).redeemWith(_ => IO(defaultValue), x => IO(x * x))

      ioWithException shouldBe a[IO[Int]]
      ioWithException.unsafeRunSync() shouldBe defaultValue
    }

    "redeemWith should transform result of source on evaluation if exception not occur" in {
      val defaultValue = 0
      val ioString = IO("10")
      val ioWithException = ioString.map(_.toInt).redeemWith(_ => IO(defaultValue), x => IO(x * x))

      ioWithException shouldBe a[IO[Int]]
      ioWithException.unsafeRunSync() shouldBe 100
    }
  }
}
