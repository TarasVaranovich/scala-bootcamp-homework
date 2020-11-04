package effects

import effects.EffectsHomework1.IO
import org.scalatest.flatspec.AnyFlatSpec

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.Try

class EffectsHomework1Spec extends AnyFlatSpec {
  private val ONE_VAL: Int = 1
  private val DIVIDE_BY_ZERO: String = "/ by zero"
  private val MESSAGE: String = "Successful result"
  private implicit val executionContext = ExecutionContext.global

  class Container(msg: String) {
    def message: String = msg
  }
  case class IntContainer(numberVal: Int, description: String) extends Container(description)

  "map operation" should "successfully convert IO value to string" in {
    assert(IO(ONE_VAL).map(_.toString).unsafeRunSync() == "1")
  }

  "map operation" should "chaining exception" in {
    assertThrows[NumberFormatException](IO("not a number").map(_.toInt).unsafeRunSync() == 1)
  }

  "flatMap operation" should "successfully chain calculations" in {
    assert(IO(1).flatMap(value => IO(s"incremented: ${value + 1}")).unsafeRunSync() == "incremented: 2")
  }

  "flatMap operation" should "chaining exception" in {
    assertThrows[ArithmeticException](IO(1).flatMap(value => IO(value / 0)).unsafeRunSync())
  }

  "chaining IO by *> operation" should "return last result" in {
    assert((IO(ONE_VAL) *> IO(MESSAGE)).unsafeRunSync() == MESSAGE)
  }

  "unsuccessfully chaining IO by *> operation" should "chaining exception" in {
    assertThrows[ArithmeticException]((IO(ONE_VAL / 0) *> IO(MESSAGE)).unsafeRunSync())
  }

  "as operation" should "successfully replace IO result" in {
    assert((IO(ONE_VAL).as(MESSAGE).unsafeRunSync() == MESSAGE))
  }

  "as operation" should "chaining exception" in {
    assertThrows[ArithmeticException]((IO(ONE_VAL / 0).as(MESSAGE).unsafeRunSync()))
  }

  "void operation" should "return 'unit' value" in {
    assert(IO(ONE_VAL).void.unsafeRunSync() == ())
  }

  "successful attempt" should "return 'right' with value" in {
    assert(IO(ONE_VAL).attempt.unsafeRunSync() == Right(ONE_VAL))
  }

  "failure attempt" should "return 'left' with Arithmetic exception" in {
    assert(IO(ONE_VAL / 0).attempt.unsafeRunSync().swap.getOrElse(throw new Error).isInstanceOf[ArithmeticException])
  }

  "successful option operation" should "return 'some' with value" in {
    assert(IO(ONE_VAL).option.unsafeRunSync().contains(ONE_VAL))
  }

  "failed options operation" should "return 'none'" in {
    assert(IO(ONE_VAL / 0).option.unsafeRunSync().isEmpty)
  }

  "handleErrorWith" should "return value in case of success" in {
    assert(IO(ONE_VAL).handleErrorWith((_: Throwable) => IO(0)).unsafeRunSync() == ONE_VAL)
  }

  "handleErrorWith" should "recover with value in case of error" in {
    assert(IO(ONE_VAL / 0).handleErrorWith((_: Throwable) => IO(0)).unsafeRunSync() == 0)
  }

  "handleErrorWith" should "return current class field in case of success" in {
    val recover = (ex: Throwable) => IO(new Container(ex.getMessage))
    val io: IO[IntContainer] = IO(IntContainer(ONE_VAL, MESSAGE))
    assert(io.handleErrorWith(recover).unsafeRunSync().message == MESSAGE)
  }

  "handleErrorWith" should "return parent class field in case of error" in {
    val recover = (ex: Throwable) => IO(new Container(ex.getMessage))
    val io: IO[IntContainer] = IO(IntContainer(ONE_VAL / 0, MESSAGE))
    assert(io.handleErrorWith(recover).unsafeRunSync().message == DIVIDE_BY_ZERO)
  }

  "successful redeem operation" should "return value as a string" in {
    assert(IO(ONE_VAL).redeem(e => e.getMessage, result => result.toString).unsafeRunSync() == "1")
  }

  "failure redeem operation" should "return error message as string" in {
    assert(IO(ONE_VAL / 0).redeem(e => e.getMessage, result => result.toString).unsafeRunSync() == DIVIDE_BY_ZERO)
  }

  "redeemWith" should "return success value in message encoded after successful invocation" in {
    val recover = (ex: Throwable) => IO(ex.getMessage)
    val bind = (value: Int) => IO(s"Number: $value")
    assert(IO(ONE_VAL).redeemWith(recover, bind).unsafeRunSync() == "Number: 1")
  }

  "redeemWith" should "return error message after error invocation" in {
    val recover = (ex: Throwable) => IO(ex.getMessage)
    val bind = (value: Int) => IO(s"Number: $value")
    assert(IO(ONE_VAL / 0).redeemWith(recover, bind).unsafeRunSync() == DIVIDE_BY_ZERO)
  }

  "unsafe to Future" should "return successful result" in {
    val future: Future[Int] = IO.apply(ONE_VAL).unsafeToFuture()
    assert(Await.result(future, 30 seconds) == ONE_VAL)
  }

  "unsafe to Future" should "return exception" in {
    val future: Future[Int] = IO.apply(ONE_VAL / 0).unsafeToFuture()
    assertThrows[ArithmeticException](Await.result(future, 30 seconds))
  }

  "IO created by 'apply'" should "return constructor parameter" in {
    assert(IO.apply(ONE_VAL).unsafeRunSync() == ONE_VAL)
  }

  "IO created by 'suspend'" should "return constructor parameter" in {
    assert(IO.suspend(IO(ONE_VAL)).unsafeRunSync() == ONE_VAL)
  }

  "suspend operation" should "chaining exception" in {
    assertThrows[ArithmeticException](IO.suspend(IO(ONE_VAL / 0)).unsafeRunSync())
  }

  "IO created by 'delay'" should "return constructor parameter" in {
    assert(IO.delay(ONE_VAL).unsafeRunSync() == ONE_VAL)
  }

  "IO created by 'pure'" should "return constructor parameter" in {
    assert(IO.pure(ONE_VAL).unsafeRunSync() == ONE_VAL)
  }

  "fromEither" should "return value in case of 'right'" in {
    assert(IO.fromEither(Either.cond(true, ONE_VAL, new ArithmeticException)).unsafeRunSync() == ONE_VAL)
  }

  "fromEither" should "throw exception in case of 'left'" in {
    assertThrows[ArithmeticException](IO
      .fromEither(Either.cond(false, ONE_VAL, new ArithmeticException)).unsafeRunSync())
  }

  "fromOption" should "return value in case of 'some'" in {
    assert(IO.fromOption(Some(ONE_VAL))(new ArithmeticException).unsafeRunSync() == ONE_VAL)
  }

  "fromOption" should "throw exception in case of 'none'" in {
    assertThrows[ArithmeticException](IO.fromOption(None)(new ArithmeticException).unsafeRunSync())
  }

  "fromTry" should "return value in case of 'success'" in {
    assert(IO.fromTry(Try(ONE_VAL)).unsafeRunSync() == ONE_VAL)
  }

  "fromTry" should "chain exception in case of 'failure'" in {
    assertThrows[ArithmeticException](IO.fromTry(Try(ONE_VAL / 0)).unsafeRunSync())
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