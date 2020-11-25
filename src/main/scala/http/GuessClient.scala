package http

import cats.effect.{ExitCode, IO, IOApp}
import http.GuessServer.{GameResult, InitGameRequest, InitGameResponse, ResultResponse}
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import org.http4s.circe.CirceEntityCodec.{circeEntityDecoder, circeEntityEncoder}
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits.http4sLiteralsSyntax
import org.http4s.{Header, Method, Request}

import scala.concurrent.ExecutionContext
import scala.util.Random

object GuessClient extends IOApp {
  private val uri = uri"http://localhost:9001"

  private val Min: Int = 1
  private val Max: Int = 6
  private val Attempts: Int = 7

  val initGameRequest: Request[IO] = new Request()
    .withMethod(Method.POST)
    .withUri(uri)
    .withPathInfo("/play")
    .withEntity(InitGameRequest(Min, Max, Attempts))


  def guess(token: String, number: Int): Request[IO] = new Request()
    .withMethod(Method.GET)
    .withUri(uri)
    .withPathInfo(s"/guess/$number")
    .withHeaders(Header.apply("player-id", token))

  def play(token: String, client: Client[IO]): IO[Unit] =
    for {
      number <- IO(Random.between(Min, Max))
      guess <- client.expect[ResultResponse](guess(token, number))
      result <- IO.apply(guess.gameResult)
      _ <- if (result != GameResult.Win && result != GameResult.Lose) play(token, client) else IO(println(result))
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](ExecutionContext.global).resource.use { client =>
      for {
        initResponse <- client.expect[InitGameResponse](initGameRequest)
        _ <- play(initResponse.token, client)
      } yield ()
    }.as(ExitCode.Success)
}