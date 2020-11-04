package effects

import scala.concurrent.{Future, Promise}
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
  final class IO[A] private(private val run: () => A) {

    /**
     * Functor map on `IO`. Given a mapping function, it transforms the
     * value produced by the source, while keeping the `IO` context.
     *
     * Any exceptions thrown within the function will be caught and
     * sequenced into the `IO`, because due to the nature of
     * asynchronous processes, without catching and handling exceptions,
     * failures would be completely silent and `IO` references would
     * never terminate on evaluation.
     */
    def map[B](f: A => B): IO[B] = IO(f.apply(this.run.apply()))

    /**
     * Monadic bind on `IO`, used for sequentially composing two `IO`
     * actions, where the value produced by the first `IO` is passed as
     * input to a function producing the second `IO` action.
     *
     * Due to this operation's signature, `flatMap` forces a data
     * dependency between two `IO` actions, thus ensuring sequencing
     * (e.g. one action to be executed before another one).
     *
     * Any exceptions thrown within the function will be caught and
     * sequenced into the `IO`, because due to the nature of
     * asynchronous processes, without catching and handling exceptions,
     * failures would be completely silent and `IO` references would
     * never terminate on evaluation.
     */
    def flatMap[B](f: A => IO[B]): IO[B] = f.apply(this.run.apply())

    /**
     * Runs the current IO, then runs the parameter, keeping its result.
     * The result of the first action is ignored.
     * If the source fails, the other action won't run.
     * */
    def *>[B](another: IO[B]): IO[B] = flatMap(_ => another)

    /**
     * Replaces the result of this IO with the given value.
     * The value won't be computed if the IO doesn't succeed.
     * */
    def as[B](newValue: => B): IO[B] = map(_ => newValue)

    /**
     * Ignores the result of this IO.
     */
    def void: IO[Unit] = map(_ => ())

    /**
     * Materializes any sequenced exceptions into value space, where
     * they may be handled.
     *
     * This is analogous to the `catch` clause in `try`/`catch`, being
     * the inverse of `IO.raiseError`. Thus:
     *
     * {{{
     * IO.raiseError(ex).attempt.unsafeRunAsync === Left(ex)
     * }}}
     *
     * @see [[IO.raiseError]]
     */
    def attempt: IO[Either[Throwable, A]] = IO(Try(this.run.apply()).toEither)

    /**
     * Replaces failures in this IO with an empty Option.
     */
    def option: IO[Option[A]] = redeem(_ => None, Some(_))

    /**
     * Handle any error, potentially recovering from it, by mapping it to another
     * `IO` value.
     *
     * Implements `ApplicativeError.handleErrorWith`.
     */
    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = Try(this.run.apply()) match {
      case Success(value) => IO(value)
      case Failure(error) => f.apply(error)
    }

    /**
     * Returns a new value that transforms the result of the source,
     * given the `recover` or `map` functions, which get executed depending
     * on whether the result ends in error or if it is successful.
     *
     * This is an optimization on usage of [[attempt]] and [[map]],
     * this equivalence being true:
     *
     * {{{
     *   io.redeem(recover, map) <-> io.attempt.map(_.fold(recover, map))
     * }}}
     *
     * Usage of `redeem` subsumes `handleError` because:
     *
     * {{{
     *   io.redeem(fe, id) <-> io.handleError(fe)
     * }}}
     *
     * @param recover is a function used for error recover in case the
     *                source ends in error
     * @param map     is a function used for mapping the result of the source
     *                in case it ends in success
     */
    def redeem[B](recover: Throwable => B, map: A => B): IO[B] = Try(map(this.run.apply())) match {
      case Success(value) => IO(value)
      case Failure(exception) => IO(recover(exception))
    }

    /**
     * Returns a new value that transforms the result of the source,
     * given the `recover` or `bind` functions, which get executed depending
     * on whether the result ends in error or if it is successful.
     *
     * This is an optimization on usage of [[attempt]] and [[flatMap]],
     * this equivalence being available:
     *
     * {{{
     *   io.redeemWith(recover, bind) <-> io.attempt.flatMap(_.fold(recover, bind))
     * }}}
     *
     * Usage of `redeemWith` subsumes `handleErrorWith` because:
     *
     * {{{
     *   io.redeemWith(fe, F.pure) <-> io.handleErrorWith(fe)
     * }}}
     *
     * Usage of `redeemWith` also subsumes [[flatMap]] because:
     *
     * {{{
     *   io.redeemWith(F.raiseError, fs) <-> io.flatMap(fs)
     * }}}
     *
     * @param recover is the function that gets called to recover the source
     *                in case of error
     * @param bind    is the function that gets to transform the source
     *                in case of success
     */
    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = Try(bind(this.run.apply())) match {
      case Success(value) => value
      case Failure(exception) => recover.apply(exception)
    }

    /**
     * Produces the result by running the encapsulated effects as impure
     * side effects.
     *
     * If any component of the computation is asynchronous, the current
     * thread will block awaiting the results of the async computation.
     * On JavaScript, an exception will be thrown instead to avoid
     * generating a deadlock. By default, this blocking will be
     * unbounded.  To limit the thread block to some fixed time, use
     * `unsafeRunTimed` instead.
     *
     * Any exceptions raised within the effect will be re-thrown during
     * evaluation.
     *
     * As the name says, this is an UNSAFE function as it is impure and
     * performs side effects, not to mention blocking, throwing
     * exceptions, and doing other things that are at odds with
     * reasonable software.  You should ideally only call this function
     * *once*, at the very end of your program.
     */
    def unsafeRunSync(): A = this.run.apply()

    /**
     * Evaluates the effect and produces the result in a `Future`.
     *
     * This is similar to `unsafeRunAsync` in that it evaluates the `IO`
     * as a side effect in a non-blocking fashion, but uses a `Future`
     * rather than an explicit callback.  This function should really
     * only be used if interoperating with legacy code which uses Scala
     * futures.
     *
     * see IO.fromFuture
     */
    def unsafeToFuture(): Future[A] = Promise.fromTry(Try(unsafeRunSync())).future
  }

  object IO {
    /**
     * Suspends a synchronous side effect in `IO`.
     *
     * Alias for `IO.delay(body)`.
     */
    def apply[A](body: => A): IO[A] = delay(body)

    /**
     * Suspends a synchronous side effect which produces an `IO` in `IO`.
     *
     * This is useful for trampolining (i.e. when the side effect is
     * conceptually the allocation of a stack frame).  Any exceptions
     * thrown by the side effect will be caught and sequenced into the
     * `IO`.
     */
    def suspend[A](thunk: => IO[A]): IO[A] = thunk

    /**
     * Suspends a synchronous side effect in `IO`.
     *
     * Any exceptions thrown by the effect will be caught and sequenced
     * into the `IO`.
     */
    def delay[A](body: => A): IO[A] = new IO(() => body)

    /**
     * Suspends a pure value in `IO`.
     *
     * This should ''only'' be used if the value in question has
     * "already" been computed!  In other words, something like
     * `IO.pure(readLine)` is most definitely not the right thing to do!
     * However, `IO.pure(42)` is correct and will be more efficient
     * (when evaluated) than `IO(42)`, due to avoiding the allocation of
     * extra thunks.
     */
    def pure[A](a: A): IO[A] = IO(a)

    /**
     * Lifts an `Either[Throwable, A]` into the `IO[A]` context, raising
     * the throwable if it exists.
     */
    def fromEither[A](e: Either[Throwable, A]): IO[A] = e match {
      case Right(value) => pure(value)
      case Left(error) => raiseError(error)
    }

    /**
     * Lifts an `Option[A]` into the `IO[A]` context, raising the throwable if the option is empty.
     */
    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = option match {
      case Some(value) => pure(value)
      case None => raiseError(orElse)
    }

    /**
     * Lifts an `Try[A]` into the `IO[A]` context, raising the throwable if it
     * exists.
     */
    def fromTry[A](t: Try[A]): IO[A] = t match {
      case Success(value) => pure(value)
      case Failure(exception) => raiseError(exception)
    }

    /**
     * An IO that contains an empty Option.
     */
    def none[A]: IO[Option[A]] = pure(None)

    /**
     * Constructs an `IO` which sequences the specified exception.
     *
     * If this `IO` is run using `unsafeRunSync` or `unsafeRunTimed`,
     * the exception will be thrown.  This exception can be "caught" (or
     * rather, materialized into value-space) using the `attempt`
     * method.
     *
     * @see [[IO#attempt]]
     */
    def raiseError[A](e: Throwable): IO[A] = IO(throw e)

    /**
     * Returns `raiseError` when `cond` is false, otherwise IO.unit
     *
     * @example {{{
     * val tooMany = 5
     * val x: Int = ???
     * IO.raiseUnless(x < tooMany)(new IllegalArgumentException("Too many"))
     * }}}
     */
    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = IO.unlessA(cond)(IO.raiseError(e))

    /**
     * Returns `raiseError` when the `cond` is true, otherwise `IO.unit`
     *
     * @example {{{
     * val tooMany = 5
     * val x: Int = ???
     * IO.raiseWhen(x >= tooMany)(new IllegalArgumentException("Too many"))
     * }}}
     */
    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = IO.whenA(cond)(IO.raiseError(e))

    /**
     * Returns the given argument if `cond` is false, otherwise `IO.Unit`
     *
     * @see [[IO.whenA]] for the inverse
     * @see [[IO.raiseWhen]] for conditionally raising an error
     */
    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) unit else action

    /**
     * Returns the given argument if `cond` is true, otherwise `IO.Unit`
     *
     * @see [[IO.unlessA]] for the inverse
     * @see [[IO.raiseWhen]] for conditionally raising an error
     */
    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) action else unit

    /** Alias for `IO.pure(())`. */
    val unit: IO[Unit] = pure(())
  }
}