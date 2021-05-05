package com.evolutiongaming.bootcamp.http

import cats.effect._
import cats.syntax.all._
import io.circe.generic.JsonCodec
import org.http4s._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

import java.util.UUID
import scala.concurrent.ExecutionContext.global
import scala.util.Random

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
  import org.http4s.circe.CirceEntityCodec._

  private def randomNumber(min: Int, max: Int): Int = {
    val random = new Random
    min + random.nextInt((max - min) + 1)
  }

  private var players = Set.empty[Player]
  private var playersAndNumbers = Map.empty[UUID, UserGuess]

  val guessTrying = 7

  val guessGameServer = HttpRoutes
    .of[IO] {
      case req @ POST -> Root / "start" =>
        req.as[StartGameRequest].flatMap { startGameRequest =>
          val player = Player(UUID.randomUUID(), startGameRequest.name)
          players = players + player
          val guessedNumber = randomNumber(startGameRequest.range.from, startGameRequest.range.to)
          playersAndNumbers += (player.id -> UserGuess(guessedNumber, guessTrying))

          Ok(
            StartGameResponse(
              s"Welcome, brave ${startGameRequest.name}! I guess a number and you have $guessTrying trying",
              guessTrying,
              player.id
            )
          )
        }
      case req @ POST -> Root / "guess" =>
        req.as[GuessRequest].flatMap { guessRequest =>
          val playerId = guessRequest.playerId

          playersAndNumbers.get(playerId) match {
            case Some(userGuess) =>
              val guessedNumber = userGuess.number
              if (guessRequest.number == guessedNumber)
                Ok(
                  GuessResponse(
                    s"Yeah, it's $guessedNumber! You won!",
                    guess = true,
                    gameEnded = true,
                    isBiggerThanGuessed = false
                  )
                )
              else if (userGuess.guessTrying == 1) {
                Ok(
                  GuessResponse(
                    s"You have not guess trying. You loose. The number was $guessedNumber",
                    guess = false,
                    gameEnded = true,
                    isBiggerThanGuessed = false
                  )
                )
              } else {
                val remainGuessTrying = userGuess.guessTrying - 1
                playersAndNumbers += (playerId -> UserGuess(guessedNumber, remainGuessTrying))

                val isBiggerThatGuessedNumber: Boolean = guessRequest.number >= guessedNumber

                Ok(
                  GuessResponse(
                    s"No, it's number ${if (isBiggerThatGuessedNumber) "bigger" else "lower"} than I guess. " +
                      s"You have guess trying: $remainGuessTrying",
                    guess = false,
                    gameEnded = false,
                    isBiggerThanGuessed = isBiggerThatGuessedNumber
                  )
                )
              }
            case None => NotFound(s"Player with this id: $playerId does not exist")
          }
        }
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
  final case class Player(id: UUID, name: String)
  @JsonCodec final case class Range(from: Int, to: Int)
  @JsonCodec final case class StartGameRequest(name: String, range: Range)
  @JsonCodec final case class StartGameResponse(welcomeMessage: String, guessCount: Int, playerId: UUID)
  @JsonCodec final case class GuessRequest(playerId: UUID, number: Int)
  @JsonCodec final case class GuessResponse(message: String, guess: Boolean, gameEnded: Boolean, isBiggerThanGuessed: Boolean)

  final case class UserGuess(number: Int, guessTrying: Int)
}

object GuessClient extends IOApp {
  import Game._
  import org.http4s.circe.CirceEntityCodec._

  private val uri = uri"http://localhost:9001"

  override def run(args: List[String]): IO[ExitCode] = {
    var min = 1
    var max = 15
    val initNumber = (min + max) / 2

    def loop(playerId: UUID, guessNumber: Int): IO[GuessResponse] = {
      println()
      println(guessNumber)
      tryToGuess(playerId, guessNumber).flatMap { resp =>
        if (!isGameFinished(resp)) {
          if (resp.isBiggerThanGuessed) {
            max = guessNumber
            loop(playerId, guessNumber / 2)
          } else {
            min = guessNumber
            loop(playerId, (max + guessNumber) / 2)
          }
        } else {
          resp.pure[IO]
        }
      }
    }

    BlazeClientBuilder[IO](global).resource.use { _ =>
      for {
        startGameResponse0 <- initGame("Yaroslav", min, max)
        startGameResponse1 <- initGame("Marie", min, max)
        startGameResponse2 <- initGame("Pro", min, max)
        _ <- loop(startGameResponse0.playerId, initNumber) >>= (resp => IO(println(resp)))
        _ <- loop(startGameResponse1.playerId, initNumber) >>= (resp => IO(println(resp)))
        _ <- loop(startGameResponse2.playerId, initNumber) >>= (resp => IO(println(resp)))
      } yield ()
    }.as(ExitCode.Success)
  }

  def initGame(name: String, from: Int, to: Int): IO[StartGameResponse] =
    BlazeClientBuilder[IO](global).resource.use { client =>
      client.expect[StartGameResponse](Method.POST(StartGameRequest(name, Range(from, to)), uri / "start"))
    }

  def tryToGuess(playerId: UUID, number: Int): IO[GuessResponse] =
    BlazeClientBuilder[IO](global).resource.use { client =>
      client.expect[GuessResponse](Method.POST(GuessRequest(playerId, number), uri / "guess"))
    }

  def isGameFinished(guessResponse: GuessResponse): Boolean =
    guessResponse.guess || guessResponse.gameEnded
}
