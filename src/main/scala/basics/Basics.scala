package basics

import scala.annotation.tailrec

object Basics {

  /**
   * Function calculates last common multiple.
   *
   * @param a
   * @param b
   * @return least number which can be divided without reminder a and b.
   *         Throws exception in case if parameters to large
   */
  def lcm(a: Int, b: Int): Int = {
    if (a == 0 || b == 0) {
      throw new Exception(s"Cannot calculate lcm for $a and $b.")
    }
    if ((BigInt(a) * BigInt(b)).abs.isValidInt) {
      throw new Exception("Result not predictable. Function doesn't work with BigInt.")
    }
    val maxValue: Int = scala.math.max(a, b).abs
    val minValue: Int = scala.math.min(a, b).abs

    def lcmCalculator(minValue: Int, increment: Int, candidate: Int): Int = {
      if (candidate % minValue == 0) candidate else lcmCalculator(minValue, increment, candidate + increment)
    }

    lcmCalculator(minValue, maxValue, maxValue)
  }

  /**
   * Function calculates gratest common divisor
   */
  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }

}
