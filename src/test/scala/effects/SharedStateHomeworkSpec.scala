package effects

import java.util.concurrent.Executors

import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{ContextShift, ExitCode, IO}
import effects.SharedStateHomework.Cache
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

class SharedStateHomeworkSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "should successfully leave two caches" in {
    val exitCode = for {
      cache <- Cache.of[IO, Int, String](4.seconds, 3.seconds)
      _ <- cache.put(1, "Hello")
      _ <- cache.put(2, "World")
      _ <- IO.sleep(3.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        assert(s.isDefined)
      })
      _ <- cache.get(2).flatMap(s => IO {
        assert(s.isDefined)
      })
    } yield ExitCode.Success
    assert(exitCode.unsafeRunSync() == ExitCode.Success)
  }

  "should successfully evict two caches" in {
    val exitCode = for {
      cache <- Cache.of[IO, Int, String](4.seconds, 3.seconds)
      _ <- cache.put(1, "Hello")
      _ <- cache.put(2, "World")
      _ <- IO.sleep(6.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        assert(s.isEmpty)
      })
      _ <- cache.get(2).flatMap(s => IO {
        assert(s.isEmpty)
      })
    } yield ExitCode.Success
    assert(exitCode.unsafeRunSync() == ExitCode.Success)
  }

  "should successfully renew key expiration" in {
    val exitCode = for {
      cache <- Cache.of[IO, Int, String](4.seconds, 3.seconds)
      _ <- cache.put(1, "Hello")
      _ <- cache.put(2, "World")
      _ <- IO.sleep(3.seconds)
      _ <- cache.put(2, "World")
      _ <- IO.sleep(3.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        assert(s.isEmpty)
      })
      _ <- cache.get(2).flatMap(s => IO {
        assert(s.isDefined)
      })
    } yield ExitCode.Success
    assert(exitCode.unsafeRunSync() == ExitCode.Success)
  }

  "should successfully provide concurrent access" in {
    val exitCode = for {
      cache <- Cache.of[IO, Int, String](15.seconds, 10.seconds)
      _ <- cache.put(1, "1")
      _ <- ContextShift[IO].evalOn(ExecutionContext.fromExecutor(Executors.newFixedThreadPool(1)))(
        cache.put(2, nextValue(1, "1", cache)))
      _ <- ContextShift[IO].evalOn(ExecutionContext.fromExecutor(Executors.newFixedThreadPool(1)))(
        cache.put(3, nextValue(1, "1", cache)))
      _ <- ContextShift[IO].evalOn(ExecutionContext.fromExecutor(Executors.newFixedThreadPool(1)))(
        cache.put(4, nextValue(1, "1", cache)))
      _ <- ContextShift[IO].evalOn(ExecutionContext.fromExecutor(Executors.newFixedThreadPool(1)))(
        cache.put(5, nextValue(1, "1", cache)))
      _ <- cache.get(2).flatMap(value => IO {
        assert(value.contains("2"))
      })
      _ <- cache.get(3).flatMap(value => IO {
        assert(value.contains("3"))
      })
      _ <- cache.get(4).flatMap(value => IO {
        assert(value.contains("4"))
      })
      _ <- cache.get(5).flatMap(value => IO {
        assert(value.contains("5"))
      })
    } yield ExitCode.Success
    assert(exitCode.unsafeRunSync() == ExitCode.Success)
  }

  "should be referential transparent" in {
    val cacheIO = Cache.of[IO, Int, String](15.seconds, 10.seconds)
    val exitCodeFirst = for {
      cache <- cacheIO
      _ <- cache.put(1, "Hello")
      _ <- cache.get(1).flatMap(s => IO {
        assert(s.isDefined)
      })
    } yield ExitCode.Success

    val exitCodeSecond = for {
      cache <- cacheIO
      _ <- cache.get(1).flatMap(s => IO {
        assert(s.isEmpty)
      })
    } yield ExitCode.Success
    assert(exitCodeFirst.unsafeRunSync() == ExitCode.Success)
    assert(exitCodeSecond.unsafeRunSync() == ExitCode.Success)
  }

  def nextValue(key: Int, value: String, cache: Cache[IO, Int, String]): String =
    if (cache.get(key).unsafeRunSync().isDefined) {
      nextValue(key + 1, (value.toInt + 1).toString, cache)
    } else value
}