package basics

import basics.ControlStructures.Command._

import scala.io.Source

object ControlStructures {
  private val MIN_OPERANDS: Int = 2
  private val PARSE_ERROR: String = "Cannot parse '?' command"

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
    line.split(" ").toList.map(_.trim).filter(_.nonEmpty) match {
      case "divide" :: operands => operands.head.toDoubleOption
        .flatMap(dividend =>
          operands(1).toDoubleOption
            .withFilter(divisor => (divisor != 0))
            .map(divisor => Divide(dividend, divisor))
        ).fold[Either[ErrorMessage, Command]](
        Left(ErrorMessage(PARSE_ERROR.replace("?", Divide.getClass.getSimpleName))))(Right.apply)

      case "sum" :: operands => Either.cond(
        operands.map(_.toDoubleOption).count(_.isDefined) > MIN_OPERANDS,
        Sum(operands.map(_.toDoubleOption).filter(_.isDefined).map(_.get)),
        ErrorMessage(PARSE_ERROR.replace("?", Sum.getClass.getSimpleName)))

      case "average" :: operands => Either.cond(
        operands.map(_.toDoubleOption).count(_.isDefined) > MIN_OPERANDS,
        Average(operands.map(_.toDoubleOption).filter(_.isDefined).map(_.get)),
        ErrorMessage(PARSE_ERROR.replace("?", Average.getClass.getSimpleName)))

      case "min" :: operands => Either.cond(
        operands.map(_.toDoubleOption).count(_.isDefined) > MIN_OPERANDS,
        Min(operands.map(_.toDoubleOption).filter(_.isDefined).map(_.get)),
        ErrorMessage(PARSE_ERROR.replace("?", Min.getClass.getSimpleName)))

      case "max" :: tail => Either.cond(
        tail.map(_.toDoubleOption).count(_.isDefined) > MIN_OPERANDS,
        Max(tail.map(_.toDoubleOption).filter(_.isDefined).map(_.get)),
        ErrorMessage(PARSE_ERROR.replace("?", Max.getClass.getSimpleName)))

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
