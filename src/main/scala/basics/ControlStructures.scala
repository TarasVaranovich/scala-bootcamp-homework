package basics

import basics.ControlStructures.Command._

import scala.io.Source

object ControlStructures {

  sealed trait Formatted {

    def formatted: String

    protected def formatDouble(value: Double, format: Double => String): String = {
      if (Math.floor(value) == value) value.toInt.toString else format(value)
    }
  }

  sealed trait Command extends Formatted {

    protected def concatParams(params: List[Double], format: Double => String): String =
      params.map(value => formatDouble(value, format))
        .reduce((previous, next) => previous + " " + next)
  }

  object Command {

    final case class Divide(dividend: Double, divisor: Double) extends Command {
      override def formatted: String =
        s"${formatDouble(dividend, _.toString)} divided by ${formatDouble(divisor, _.toString)}"
    }

    final case class Sum(numbers: List[Double]) extends Command {
      override def formatted: String = s"the sum of ${concatParams(numbers, _.toString)}"
    }

    final case class Average(numbers: List[Double]) extends Command {
      override def formatted: String = s"the average of ${concatParams(numbers, _.toString)}"
    }

    final case class Min(numbers: List[Double]) extends Command {
      override def formatted: String = s"the minimum of ${concatParams(numbers, _.toString)}"
    }

    final case class Max(numbers: List[Double]) extends Command {
      override def formatted: String = s"the maximum of ${concatParams(numbers, _.toString)}"
    }
  }

  final case class ErrorMessage(value: String)

  sealed trait Result extends Formatted {

    protected def restrictDigits(value: Double): String =
      if (value.toString.split("\\.").last.length > 3) f"$value%1.3f" else value.toString
  }

  final case class DivideResult(value: Double) extends Result {
    override def formatted: String = formatDouble(value, restrictDigits)
  }

  final case class SumResult(value: Double) extends Result {
    override def formatted: String = formatDouble(value, _.toString)
  }

  final case class AverageResult(value: Double) extends Result {
    override def formatted: String = formatDouble(value, restrictDigits)
  }

  final case class MinResult(value: Double) extends Result {
    override def formatted: String = formatDouble(value, _.toString)
  }

  final case class MaxResult(value: Double) extends Result {
    override def formatted: String = formatDouble(value, _.toString)
  }

  /**
   * Function creates command from string.
   * String must contain not less than '2' parameters for operation sense
   * If string contains not numeric symbols mixed with numbers
   * command will be parsed despite them
   *
   * @param line string with decoded operation
   * @return "valid/checked" class which implements {@link Command}
   */
  def parseCommand(line: String): Either[ErrorMessage, Command] = {
    val command: String = line.trim.split(" ").head.trim

    val operands: List[Double] = line.trim.split(" ")
      .toList
      .map(_.trim)
      .map(_.toDoubleOption)
      .flatMap(_.toList)

    val buildResult =
      (when: List[Double] => Boolean, than: List[Double] => Command) =>
        Either.cond(
          when(operands),
          than(operands),
          ErrorMessage(s"Cannot parse '$command' command."))

    command match {
      case "divide" => buildResult(ops => (ops.size == 2 && ops.last != 0), ops => Divide(ops.head, ops.last))
      case "sum" => buildResult(_.nonEmpty, Sum)
      case "average" => buildResult(_.nonEmpty, Average)
      case "min" => buildResult(_.nonEmpty, Min)
      case "max" => buildResult(_.nonEmpty, Max)
      case command => Left(ErrorMessage(s"Unknown command '$command'"))
    }
  }

  def calculate(command: Command): Either[ErrorMessage, Result] = {
    try {
      command match {
        case divide: Divide => Right(DivideResult(divide.dividend / divide.divisor))
        case sum: Sum => Right(SumResult(sum.numbers.sum))
        case average: Average => Right(AverageResult(average.numbers.sum / average.numbers.size))
        case min: Min => Right(MinResult(min.numbers.min))
        case max: Max => Right(MaxResult(max.numbers.max))
        case _ => Left(ErrorMessage(s"Unknown operation."))
      }
    } catch {
      case ex: Exception => Left(ErrorMessage(s"Exception in calculation '${ex.getMessage}'."))
    }
  }

  def render(command: Command)(result: Result): String = s"${command.formatted} is ${result.formatted}"

  def process(line: String): String = {
    import cats.implicits._
    (for {
      command <- parseCommand(line)
      result <- calculate(command)
    } yield render(command)(result))
      .leftMap(message => s"Error: ${message.value}").merge
  }

  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}