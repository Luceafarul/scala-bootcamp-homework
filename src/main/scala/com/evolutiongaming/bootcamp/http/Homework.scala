package com.evolutiongaming.bootcamp.http

import cats.syntax.all._
import cats.effect._
import io.circe.generic.JsonCodec
import org.http4s._
import org.http4s.client.dsl.io._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze._
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext.global

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// The exact protocol and message format to use is not specified and should be designed while working on the task.
object GuessServer extends IOApp {
  import Game._
  import io.circe.generic.auto._
  import org.http4s.circe.CirceEntityCodec._

  val guessGameServer = HttpRoutes
    .of[IO] {
      case req @ POST -> Root / "start" =>
        req.as[StartGame].flatMap { start =>
          val randomInt = 7
          // Ya zagadal number for you and you have ... popitok
          val gameStarted = GameStarted(s"Welcome, brave ${start.user}! I guess a number and you have $randomInt trying", randomInt)
          Ok(gameStarted)
        }
//      case req @ POST -> Root / "guess" => req.as[GuessRequest].flatMap { guessRequest =>
//
//      }
    }
    .orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](global)
      .bindHttp(9001, "localhost")
      .withHttpApp(guessGameServer)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}

object Game {
  // TODO: add validation min < max, user is unique
  @JsonCodec final case class StartGame(user: String, min: Int, max: Int)
  @JsonCodec final case class GameStarted(welcomeMessage: String, guessCount: Int)
  @JsonCodec final case class GuessRequest(user: String, number: Int)
  @JsonCodec final case class GuessResponse(message: String)
}

object GuessClient extends IOApp {
  import Game._
  import io.circe.generic.auto._
  import org.http4s.circe.CirceEntityCodec._

  private val uri = uri"http://localhost:9001"

  override def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](global).resource
      .parZip(Blocker[IO])
      .use {
        case (client, blocker) =>
          for {
            _ <- client.expect[GameStarted](Method.POST(StartGame("Yaroslav", 1, 10), uri / "start")) >>= (s => IO(println(s)))
          } yield ()
      }
      .as(ExitCode.Success)
}
