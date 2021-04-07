import cats.effect.IO

val ioa = IO { println("hey!") }

val program: IO[Unit] =
  for {
    _ <- ioa
    _ <- ioa
  } yield ()

program.unsafeRunSync()

def addToGaugeIO(n: Int): IO[Unit] = IO { println(n) }

val example01 = for {
  _ <- addToGaugeIO(32)
  _ <- addToGaugeIO(32)
} yield ()

example01.unsafeRunSync()

val taskIO = addToGaugeIO(32)
val example02 = for {
  _ <- taskIO
  _ <- taskIO
} yield ()

example02.unsafeRunSync()

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

def addToGaugeFuture(n: Int): Future[Unit] = Future { println(n) }

val example03 = for {
  _ <- addToGaugeFuture(32)
  _ <- addToGaugeFuture(32)
} yield ()


val taskFuture = addToGaugeFuture(32)
val example04 = for {
  _ <- taskFuture
  _ <- taskFuture
} yield ()

def fib(n: Int, a: Long = 0, b: Long = 1): IO[Long] =
  IO(a + b).flatMap { b2 =>
    if (n > 0) fib(n - 1, b, b2)
    else IO.pure(a)
  }

fib(55).unsafeRunSync()
