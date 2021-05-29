package com.evolutiongaming.bootcamp.http.websocket

import cats.effect._
import fs2.Pipe
import fs2.concurrent.Queue
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame

import scala.concurrent.ExecutionContext.global
import scala.util.Random

// Use websocket to solve this homework
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
object GuessServerWS extends IOApp {

  private def randomNumber(min: Int, max: Int): Int = {
    val random = new Random
    min + random.nextInt((max - min) + 1)
  }

  val guessGameServer = HttpRoutes
    .of[IO] {
      case GET -> Root / "game" =>
        val guessedNumber = randomNumber(0, 100)
        val welcomePipe: Pipe[IO, WebSocketFrame, WebSocketFrame] = _.collect {
          case WebSocketFrame.Text("again\n", _) =>
            WebSocketFrame.Text("I guessed number between 0 and 100, for try to guess just send me number")
          case WebSocketFrame.Text(s, _) =>
            if (s.trim.forall(_.isDigit) && s.trim.length <= 3) {
              if (s.trim.toInt == guessedNumber) WebSocketFrame.Text(s"Congratulation! You won! It's $guessedNumber")
              else if (s.trim.toInt > guessedNumber) WebSocketFrame.Text(s"Your number is big, try again")
              else WebSocketFrame.Text(s"Your number is small, try again")
            } else WebSocketFrame.Text("You send me not a number or too big int (please send number from 0 to 100)")
        }

        for {
          queue <- Queue.unbounded[IO, WebSocketFrame]
          response <- WebSocketBuilder[IO].build(
            receive = queue.enqueue.compose(s => s.cons1(WebSocketFrame.Text("again\n"))),
            send = queue.dequeue
              .through(welcomePipe)
          )
        } yield response
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
