package http

import cats.effect.{ExitCode, IO, IOApp}
import effects.SharedStateHomework.Cache
import io.circe
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
  private val CacheAbsent: String = s"Cache not initialized."
  private val GameAbsent: String = s"Game info not found."
  private val cacheManager: CacheManager = new CacheManager()

  class CacheManager {
    private var cacheOpt: Option[Cache[IO, String, GameInfo]] = None
    def apply(cache: Cache[IO, String, GameInfo]): IO[Unit] = {
      this.cacheOpt = Some(cache)
      IO.unit
    }

    def get(key: String): IO[GameInfo] = cacheOpt match {
      case Some(cache) =>
        for {
          info <- cache.get(key)
          gameInfo <- IO.fromOption(info)(throw new Exception(GameAbsent))
        } yield gameInfo
      case None => IO.raiseError(throw new Exception(CacheAbsent))
    }

    def put(key: String, info: GameInfo): IO[Unit] = cacheOpt match {
      case Some(cache) => cache.put(key, info)
      case None => IO.raiseError(throw new Exception(CacheAbsent))
    }

    def updateAttempts(key: String): IO[Unit] = cacheOpt match {
      case Some(cache) => for {
        info <- cache.get(key)
        freshInfo <- IO
          .fromOption(info
            .flatMap(oldInfo =>
              Some(GameInfo(oldInfo.number, oldInfo.attempts - 1))))(throw new Exception(GameAbsent))
        _ <- cache.put(key, freshInfo)
      } yield ()
      case None => IO.raiseError(throw new Exception(CacheAbsent))
    }
  }

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

  private val gameRoutes = {
    import org.http4s.Status

    HttpRoutes.of[IO] {

      case request@POST -> Root / "play" => {
        val body: String = request.as[String].unsafeRunSync()
        val gameRequestDecoded: Either[circe.Error, InitGameRequest] = decode[InitGameRequest](body)

        gameRequestDecoded match {
          case Left(_) => {
            IO(Response(Status.BadRequest).withEntity("Cannot parse request body."))
          }
          case Right(initGameRequest) => {
            val token: String = java.util.UUID.randomUUID.toString
            GameInfo.fromRequest(initGameRequest) match {
              case Some(gameInfo) => for {
                result <- cacheManager.put(token, gameInfo).attempt
              } yield result match {
                case Right(_) => Response(Status.Created).withEntity(InitGameResponse(token, "Game started.").asJson)
                case Left(ex) => Response(Status.InternalServerError).withEntity(ex.getMessage)
              }
              case None => IO(Response(Status.BadRequest).withEntity("Invalid game data."))
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
              info <- cacheManager.get(token).attempt
              update <- cacheManager.updateAttempts(token).attempt
              freshInfo <- cacheManager.get(token).attempt
              attempts <- IO.apply(freshInfo.map(_.attempts))
              gameResult <- IO.apply(info.map(info => GameResult.from(info, candidate.toInt)))
            } yield (attempts, gameResult, update) match {
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

  private[http] val httpApp = {
    gameRoutes
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] = for {
    _ <- Cache.of[IO, String, GameInfo](15.minutes, 5.minutes).flatMap(cache => cacheManager.apply(cache))
    _ <- BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
  } yield ExitCode.Success
}