package hzuo.lscat

import scala.BigInt

object FunctionCounting {

  def multiplyRange(start: BigInt, end: BigInt): BigInt = {
    (start to end).foldLeft[BigInt](1)(_ * _)
  }

  def permutations(n: BigInt, m: BigInt) = {
    multiplyRange(n - m + 1, n)
  }

  def combinations(n: BigInt, m: BigInt) = {
    permutations(n, m) / multiplyRange(1, m)
  }

  implicit class Choose(n: BigInt) {
    def choose(m: BigInt) = combinations(n, m)
    def **(exp: BigInt) = {
      val expIntValue = exp.toInt
      if (BigInt(expIntValue) != exp) throw new IllegalArgumentException
      n pow expIntValue
    }
  }

  val one = BigInt(1)

  // turns out to be https://oeis.org/A000248
  def idempotentUnaryOps(n: BigInt): BigInt = {
    (one to n).map { i => (n choose i) * (i ** (n - i)) }.sum
  }

  def inverses(a: BigInt, b: BigInt) = {
    if (a > b) {
      0
    } else {
      val injections = permutations(b, a)
      val leftInversesPerInjection = a ** (b - a)
      injections * leftInversesPerInjection
    }
  }

}