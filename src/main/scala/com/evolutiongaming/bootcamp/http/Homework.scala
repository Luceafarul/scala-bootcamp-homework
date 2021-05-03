package com.evolutiongaming.bootcamp.http

import cats.syntax.all._
import cats.effect._
import io.circe.generic.JsonCodec
import org.http4s._
import org.http4s.client.JavaNetClientBuilder
import org.http4s.client.dsl.io._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze._
import org.http4s.server.blaze.BlazeServerBuilder

import java.util.UUID
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration.{FiniteDuration, SECONDS}
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
  import io.circe.generic.auto._
  import org.http4s.circe.CirceEntityCodec._

  private def randomNumber(min: Int, max: Int): Int = {
    val random = new Random
    min + random.nextInt((max - min) + 1)
  }

  // TODO ref and map?
  private var players = Set.empty[Player]
  private var playersAndNumbers = Map.empty[UUID, UserGuess]

  val guessTrying = 7

  val guessGameServer = HttpRoutes
    .of[IO] {
      case req @ POST -> Root / "start" =>
        req.as[StartGameRequest].flatMap { start =>
          // 1. Save new player
          // 2. Save number for guessing
          // 3. Create response
          val player = Player(UUID.randomUUID(), start.name)
          players = players + player
          playersAndNumbers += (player.id -> UserGuess(randomNumber(start.min, start.max), guessTrying))
          val gameStarted = StartGameResponse(
            s"Welcome, brave ${start.name}! I guess a number and you have $guessTrying trying",
            guessTrying,
            player.id
          )
          Ok(gameStarted)
        }
      case req @ POST -> Root / "guess" =>
        req.as[GuessRequest].flatMap { guessRequest =>
          val playerId = guessRequest.playerId

          playersAndNumbers.get(playerId) match {
            case Some(userGuess) =>
              // 1. Check user if exist -- else NotFound
              // 2. Check guess number and guess trying
              // 3. Check win condition
              // 4. Create response
              val guessedNumber = userGuess.number
              if (guessRequest.number == guessedNumber)
                Ok(GuessResponse(s"Year, it's $guessedNumber! You won!", guess = true, gameEnded = true))
              else if (userGuess.guessTrying == 1) {
                Ok(GuessResponse(s"You have not guess trying. You loose. The number was $guessedNumber", guess = false, gameEnded = true))
              } else {
                val remainGuessTrying = userGuess.guessTrying - 1
                playersAndNumbers += (playerId -> UserGuess(guessedNumber, remainGuessTrying))
                println(playersAndNumbers)
                Ok(
                  GuessResponse(
                    s"No, it's number bigger / lower than I guess. You have guess trying: $remainGuessTrying",
                    guess = false,
                    gameEnded = false
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
  // TODO: add validation min < max, user is unique
  @JsonCodec final case class Player(id: UUID, name: String)
  @JsonCodec final case class StartGameRequest(name: String, min: Int, max: Int)
  @JsonCodec final case class StartGameResponse(welcomeMessage: String, guessCount: Int, playerId: UUID)
  @JsonCodec final case class GuessRequest(playerId: UUID, playerName: String, number: Int)
  @JsonCodec final case class GuessResponse(message: String, guess: Boolean, gameEnded: Boolean)

  final case class UserGuess(number: Int, guessTrying: Int)
}

object GuessClient extends IOApp {
  import Game._
  import io.circe.generic.auto._
  import org.http4s.circe.CirceEntityCodec._

  private val uri = uri"http://localhost:9001"

  val blockingPool = Executors.newFixedThreadPool(2)
  val blocker = Blocker.liftExecutorService(blockingPool)
//  val client = JavaNetClientBuilder[IO](blocker).create

  override def run(args: List[String]): IO[ExitCode] = {
    val init = initGame("", 1, 2)
    val initNumber = 70

    def loop(guessNumber: Int): IO[GuessResponse] = {
      println()
      println(guessNumber)
      tryToGuess("Yaroslav", guessNumber).flatMap { resp =>
        if (!isGameFinished(resp)) {
          println(resp)
          loop(guessNumber + 1)
        } else {
          println(resp)
          IO(resp)
        }
      }
    }

    loop(initNumber).as(ExitCode.Success)
  }

  def initGame(name: String, start: Int, to: Int): IO[StartGameResponse] =
    BlazeClientBuilder[IO](global).resource.use { client =>
      client.expect[StartGameResponse](Method.POST(StartGameRequest("Yaroslav", 1, 10), uri / "start"))
    }

  def tryToGuess(name: String, number: Int): IO[GuessResponse] =
    BlazeClientBuilder[IO](global).resource.use { client =>
      client.expect[GuessResponse](Method.POST(GuessRequest(UUID.randomUUID(), name, number), uri / "guess"))
    }

  def isGameFinished(guessResponse: GuessResponse): Boolean =
    guessResponse.guess || guessResponse.gameEnded
}
