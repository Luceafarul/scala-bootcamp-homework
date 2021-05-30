package com.evolutiongaming.bootcamp.db

import cats.data.Kleisli
import cats.effect.{ExitCode, IO, IOApp}
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.{HttpRoutes, Request, Response}
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext.global

// Нужно написать небольшой CRUD для сущностей из лекции.
// Добавить в Book поле genre.
// Можно использовать любую библиотеку для работы с http (http4s, akka-http и т.д.).
// Create/Read all/Read one нужны для всех сущностей.
// Update можно ограничиться только Book.
//
// Бонусные очки:
// Пару фильтров для Book и/или Author (год, имя/название и т.д.)
// Возвращать BookWithAuthor вместо Book
// Delete
object Homework extends IOApp {
  private val books = Database.Books(DbTransactor.pooled[IO])

  private val bookRoutes = {
    import org.http4s.circe.CirceEntityCodec._

    HttpRoutes.of[IO] {
      case GET -> Root / "books" =>
        Ok(books.findAll())
    }
  }

  private val bookServerApp: Kleisli[IO, Request[IO], Response[IO]] = {
    bookRoutes
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](global)
      .bindHttp(9003, "localhost")
      .withHttpApp(bookServerApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}
