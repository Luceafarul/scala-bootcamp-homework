package com.evolutiongaming.bootcamp.effects

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/*
 * Homework 1. Provide your own implementation of a subset of `IO` functionality.
 *
 * Provide also tests for this functionality in EffectsHomework1Spec (which you should create).
 *
 * Refer to:
 *  - https://typelevel.org/cats-effect/datatypes/io.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO$.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO.html
 * about the meaning of each method as needed.
 *
 * There are two main ways how to implement IO:
 * - Executable encoding  - express every constructor and operator for our model in terms of its execution
 * - Declarative encoding - express every constructor and operator for our model as pure data in a recursive
 *                          tree structure
 *
 * While the real Cats Effect IO implementation uses declarative encoding, it will be easier to solve this
 * task using executable encoding, that is:
 *  - Add a `private val run: () => A` parameter to the class `IO` private constructor
 *  - Have most of the methods return a `new IO(...)`
 *
 * Ask questions in the bootcamp chat if stuck on this task.
 */
object EffectsHomework1 {
  final class IO[A](private val run: () => A) {
    def map[B](f: A => B): IO[B] = new IO[B](() => f(run()))
    def flatMap[B](f: A => IO[B]): IO[B] = f(run())
    def *>[B](another: IO[B]): IO[B] = another
    def as[B](newValue: => B): IO[B] = new IO[B](() => newValue)
    def void: IO[Unit] = new IO[Unit](() => ())
    def attempt: IO[Either[Throwable, A]] = new IO[Either[Throwable, A]](() => Try(run()).toEither)
    def option: IO[Option[A]] =
      new IO[Option[A]](() =>
        Try(run()) match {
          case Success(a) => Option(a)
          case Failure(_) => Option.empty[A]
        }
      )
    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] =
      attempt.flatMap {
        case Right(a) => IO(a)
        case Left(e)  => f(e)
      }
    def redeem[B](recover: Throwable => B, map: A => B): IO[B] =
      attempt.map {
        case Right(value) => map(value)
        case Left(error)  => recover(error)
      }
    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] =
      attempt.flatMap {
        case Right(value) => bind(value)
        case Left(error)  => recover(error)
      }
    def unsafeRunSync(): A = run()
    def unsafeToFuture(): Future[A] = Future(run())
  }

  object IO {
    def apply[A](body: => A): IO[A] = new IO[A](() => body)
    def suspend[A](thunk: => IO[A]): IO[A] = thunk
    def delay[A](body: => A): IO[A] = apply(body)
    def pure[A](a: A): IO[A] = apply(a)
    def fromEither[A](e: Either[Throwable, A]): IO[A] =
      e match {
        case Right(a)    => IO(a)
        case Left(error) => raiseError(error)
      }
    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] =
      option match {
        case Some(a) => IO(a)
        case None    => raiseError(orElse)
      }
    def fromTry[A](t: Try[A]): IO[A] =
      t match {
        case Success(a)     => IO(a)
        case Failure(error) => raiseError(error)
      }
    def none[A]: IO[Option[A]] = IO(Option.empty[A])
    def raiseError[A](e: Throwable): IO[A] = IO(throw e)
    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = raiseWhen(!cond)(e)
    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] =
      IO {
        if (!cond) raiseError(e)
        else whenA(cond)(unit)
      }
    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = whenA(!cond)(action)
    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] =
      new IO[Unit](() =>
        while (cond) {
          action
        }
      )
    val unit: IO[Unit] = new IO[Unit](() => ())
  }
}
