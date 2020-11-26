package http

import cats.effect.{ExitCode, IO, IOApp}
import effects.SharedStateHomework.Cache
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import io.circe.parser.decode
import io.circe.syntax.EncoderOps
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.io.{->, /, GET, POST, Root}
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.util.CaseInsensitiveString
import org.http4s.{Header, HttpRoutes, Response}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.util.Random


object GuessServer extends IOApp {
  final case class InitGameRequest(min: Int, max: Int, attempts: Int)
  final case class InitGameResponse(token: String, message: String)
  final case class ResultResponse(value: Int, gameResult: GameResult, attempts: Int)
  final case class GameInfo(number: Int, attempts: Int)
  object GameInfo {
    def fromRequest(request: InitGameRequest): Option[GameInfo] = {
      if ((request.min >= request.max) || request.min < 0 || request.max < 0 || request.attempts <= 0) None else {
        val number: Int = Random.between(request.min, request.max)
        Some(GameInfo(number, request.attempts))
      }
    }
  }

  sealed trait GameResult
  object GameResult {
    case object GreaterThan extends GameResult
    case object LessThan extends GameResult
    case object Win extends GameResult
    case object Lose extends GameResult
    def from(gameInfo: GameInfo, candidate: Int): GameResult =
      if (gameInfo.attempts <= 0) Lose
      else if (gameInfo.number > candidate) LessThan else if (gameInfo.number < candidate) GreaterThan else Win
  }

  def gameRoutes(cache: Cache[IO, String, GameInfo]) = {
    import org.http4s.Status

    HttpRoutes.of[IO] {
      case request@POST -> Root / "play" => {
        request
          .as[String]
          .flatMap {
            body =>
              decode[InitGameRequest](body) match {
                case Left(_) => {
                  IO(Response(Status.BadRequest).withEntity("Cannot parse request body."))
                }
                case Right(initGameRequest) => {
                  val token: String = java.util.UUID.randomUUID.toString
                  GameInfo.fromRequest(initGameRequest) match {
                    case Some(gameInfo) => for {
                      _ <- cache.put(token, gameInfo)
                    } yield Response(Status.Created).withEntity(InitGameResponse(token, "Game started.").asJson)
                    case None => IO(Response(Status.BadRequest).withEntity("Invalid game data."))
                  }
                }
              }
          }
      }

      case request
        @GET -> Root / "guess" / candidate => {
        val tokenOpt: Option[Header] = request.headers.get(CaseInsensitiveString.apply("player-id"))
        tokenOpt match {
          case Some(header) => {
            val token = header.value
            for {
              infoOpt <- cache.get(token)
              infoEither <- IO.fromOption(infoOpt)(throw new Exception("Game absent.")).attempt
              info <- IO.fromEither(infoEither)
              _ <- cache.put(token, GameInfo(info.number, info.attempts - 1))
              freshInfo <- cache.get(token)
              attemptsOpt <- IO.apply(freshInfo.map(_.attempts))
              attemptsEither <- IO.fromOption(attemptsOpt)(throw new Exception("Cannot update attempts.")).attempt
              gameResultOpt <- IO.apply(infoOpt.map(info => GameResult.from(info, candidate.toInt)))
              gameResultEither <- IO.fromOption(gameResultOpt)(throw new Exception("Cannot create game result.")).attempt
            } yield (attemptsEither, gameResultEither, infoEither) match {
              case (Right(attempts), Right(gameResult), Right(_)) =>
                Response(Status.Ok).withEntity(ResultResponse(candidate.toInt, gameResult, attempts).asJson)
              case (Left(ex), _, _) => Response(Status.NotFound).withEntity(ex.getMessage)
              case (_, Left(ex), _) => Response(Status.NotFound).withEntity(ex.getMessage)
              case (_, _, Left(ex)) => Response(Status.NotFound).withEntity(ex.getMessage)
            }
          }
          case None => IO(Response(Status.NotFound).withEntity(s"You are not logged in."))
        }
      }
    }
  }

  def httpApp(cache: Cache[IO, String, GameInfo]) = {
    gameRoutes(cache)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] = for {
    cache <- Cache.of[IO, String, GameInfo](15.minutes, 5.minutes)
    _ <- BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(httpApp(cache))
      .serve
      .compile
      .drain
  } yield ExitCode.Success
}