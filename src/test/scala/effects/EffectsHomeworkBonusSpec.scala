package effects

import effects.EffectsHomeworkBonus.IO
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.Try

class EffectsHomeworkBonusSpec extends AnyFlatSpec {

  "map operation" should "successfully convert IO value to string" in {
    assert(IO(OneVal).map(_.toString).unsafeRunSync() == "1")
  }

  "map operation" should "chaining exception" in {
    assertThrows[NumberFormatException](IO("not a number").map(_.toInt).unsafeRunSync() == 1)
  }

  "map operation" should "properly calculate in recursive call" in {
    def recursiveIncrement(io: IO[Int]): IO[Int] = {
      if (io.unsafeRunSync() < TwoBillion) {
        recursiveIncrement(io.map(_ + 1))
      } else io
    }
    assert(recursiveIncrement(IO(OneVal)).unsafeRunSync() == TwoBillion)
  }

  "flatMap operation" should "successfully chain calculations" in {
    assert(IO(1).flatMap(value => IO(s"incremented: ${value + 1}")).unsafeRunSync() == "incremented: 2")
  }

  "flatMap operation" should "chaining exception" in {
    assertThrows[ArithmeticException](IO(1).flatMap(value => IO(value / 0)).unsafeRunSync())
  }

  "flatMap operation" should "properly calculate in recursive call" in {
    def recursiveIncrement(io: IO[Int]): IO[Int] = {
      if (io.unsafeRunSync() < TwoBillion) {
        recursiveIncrement(io.flatMap(value => IO(value + 1)))
      } else io
    }
    assert(recursiveIncrement(IO(OneVal)).unsafeRunSync() == TwoBillion)
  }

  "chaining IO by *> operation" should "return last result" in {
    assert((IO(OneVal) *> IO(Message)).unsafeRunSync() == Message)
  }

  "unsuccessfully chaining IO by *> operation" should "chaining exception" in {
    assertThrows[ArithmeticException]((IO(OneVal / 0) *> IO(Message)).unsafeRunSync())
  }

  "as operation" should "successfully replace IO result" in {
    assert((IO(OneVal).as(Message).unsafeRunSync() == Message))
  }

  "as operation" should "chaining exception" in {
    assertThrows[ArithmeticException]((IO(OneVal / 0).as(Message).unsafeRunSync()))
  }

  "void operation" should "return 'unit' value" in {
    assert(IO(OneVal).void.unsafeRunSync() == ())
  }

  "successful attempt" should "return 'right' with value" in {
    assert(IO(OneVal).attempt.unsafeRunSync() == Right(OneVal))
  }

  "failure attempt" should "return 'left' with Arithmetic exception" in {
    assert(IO(OneVal / 0).attempt.unsafeRunSync().swap.getOrElse(throw new Error).isInstanceOf[ArithmeticException])
  }

  "successful option operation" should "return 'some' with value" in {
    assert(IO(OneVal).option.unsafeRunSync().contains(OneVal))
  }

  "failed options operation" should "return 'none'" in {
    assert(IO(OneVal / 0).option.unsafeRunSync().isEmpty)
  }

  "handleErrorWith" should "return value in case of success" in {
    assert(IO(OneVal).handleErrorWith((_: Throwable) => IO(0)).unsafeRunSync() == OneVal)
  }

  "handleErrorWith" should "recover with value in case of error" in {
    assert(IO(OneVal / 0).handleErrorWith((_: Throwable) => IO(0)).unsafeRunSync() == 0)
  }

  "handleErrorWith" should "return current class field in case of success" in {
    val recover = (ex: Throwable) => IO(new Container(ex.getMessage))
    val io: IO[IntContainer] = IO(IntContainer(OneVal, Message))
    assert(io.handleErrorWith(recover).unsafeRunSync().message == Message)
  }

  "handleErrorWith" should "return parent class field in case of error" in {
    val recover = (ex: Throwable) => IO(new Container(ex.getMessage))
    val io: IO[IntContainer] = IO(IntContainer(OneVal / 0, Message))
    assert(io.handleErrorWith(recover).unsafeRunSync().message == DivideByZero)
  }

  "successful redeem operation" should "return value as a string" in {
    assert(IO(OneVal).redeem(e => e.getMessage, result => result.toString).unsafeRunSync() == "1")
  }

  "failure redeem operation" should "return error message as string" in {
    assert(IO(OneVal / 0).redeem(e => e.getMessage, result => result.toString).unsafeRunSync() == DivideByZero)
  }

  "redeemWith" should "return success value in message encoded after successful invocation" in {
    val recover = (ex: Throwable) => IO(ex.getMessage)
    val bind = (value: Int) => IO(s"Number: $value")
    assert(IO(OneVal).redeemWith(recover, bind).unsafeRunSync() == "Number: 1")
  }

  "redeemWith" should "return error message after error invocation" in {
    val recover = (ex: Throwable) => IO(ex.getMessage)
    val bind = (value: Int) => IO(s"Number: $value")
    assert(IO(OneVal / 0).redeemWith(recover, bind).unsafeRunSync() == DivideByZero)
  }

  "unsafe to Future" should "return successful result" in {
    val future: Future[Int] = IO(OneVal).unsafeToFuture()
    assert(Await.result(future, 30 seconds) == OneVal)
  }

  "unsafe to Future" should "return exception" in {
    val future: Future[Int] = IO(OneVal / 0).unsafeToFuture()
    assertThrows[ArithmeticException](Await.result(future, 30 seconds))
  }

  "IO created by 'apply'" should "return constructor parameter" in {
    assert(IO(OneVal).unsafeRunSync() == OneVal)
  }

  "IO created by 'suspend'" should "return constructor parameter" in {
    assert(IO.suspend(IO(OneVal)).unsafeRunSync() == OneVal)
  }

  "suspend operation" should "chaining exception" in {
    assertThrows[ArithmeticException](IO.suspend(IO(OneVal / 0)).unsafeRunSync())
  }

  "IO created by 'delay'" should "return constructor parameter" in {
    assert(IO.delay(OneVal).unsafeRunSync() == OneVal)
  }

  "IO created by 'pure'" should "return constructor parameter" in {
    assert(IO.pure(OneVal).unsafeRunSync() == OneVal)
  }

  "fromEither" should "return value in case of 'right'" in {
    assert(IO.fromEither(Either.cond(true, OneVal, new ArithmeticException)).unsafeRunSync() == OneVal)
  }

  "fromEither" should "throw exception in case of 'left'" in {
    assertThrows[ArithmeticException](IO
      .fromEither(Either.cond(false, OneVal, new ArithmeticException)).unsafeRunSync())
  }

  "fromOption" should "return value in case of 'some'" in {
    assert(IO.fromOption(Some(OneVal))(new ArithmeticException).unsafeRunSync() == OneVal)
  }

  "fromOption" should "throw exception in case of 'none'" in {
    assertThrows[ArithmeticException](IO.fromOption(None)(new ArithmeticException).unsafeRunSync())
  }

  "fromTry" should "return value in case of 'success'" in {
    assert(IO.fromTry(Try(OneVal)).unsafeRunSync() == OneVal)
  }

  "fromTry" should "chain exception in case of 'failure'" in {
    assertThrows[ArithmeticException](IO.fromTry(Try(OneVal / 0)).unsafeRunSync())
  }

  "IO created by none" should "return 'none'" in {
    assert(IO.none.unsafeRunSync().isEmpty)
  }

  "raiseError" should "chain appropriate exception" in {
    assertThrows[ArithmeticException](IO.raiseError(new ArithmeticException).unsafeRunSync())
  }

  "'raiseUnless' in case of positive condition" should "return 'unit'" in {
    assert(IO.raiseUnless(true)(new ArithmeticException()).unsafeRunSync() == ())
  }

  "'raiseUnless' in case of negative condition" should "chain appropriate exception" in {
    assertThrows[ArithmeticException](IO.raiseUnless(false)(new ArithmeticException()).unsafeRunSync())
  }

  "'raiseWhen' in case of negative condition" should "return 'unit'" in {
    assert(IO.raiseWhen(false)(new ArithmeticException()).unsafeRunSync() == ())
  }

  "'raiseWhen' in case of positive condition" should "chain appropriate exception" in {
    assertThrows[ArithmeticException](IO.raiseWhen(true)(new ArithmeticException()).unsafeRunSync())
  }

  "IO created by 'unlessA'" should "return 'unit' value in each condition" in {
    assert(IO.unlessA(true)(IO.unit).unsafeRunSync() == ())
    assert(IO.unlessA(false)(IO.unit).unsafeRunSync() == ())
  }

  "IO created by 'whenA'" should "return 'unit' value in each condition" in {
    assert(IO.whenA(true)(IO.unit).unsafeRunSync() == ())
    assert(IO.whenA(false)(IO.unit).unsafeRunSync() == ())
  }

  "IO created by 'unit'" should "return 'unit'" in {
    assert(IO.unit.unsafeRunSync() == ())
  }
}
