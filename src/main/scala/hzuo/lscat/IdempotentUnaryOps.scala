package hzuo.lscat

import scala.BigInt
import scala.math.BigInt.int2bigInt

object IdempotentUnaryOps extends App {

  def multiplyRange(start: BigInt)(end: BigInt): BigInt = {
    (start to end).foldLeft[BigInt](1)(_ * _)
  }

  def choose(n: BigInt, m: BigInt) = {
    multiplyRange(n - m + 1)(n) / multiplyRange(1)(m)
  }

  val one = BigInt(1)

  def idempotentUnaryOps(n: BigInt): BigInt = {
    (one to n).map { i =>
      val exp = (n - i).toInt
      choose(n, i) * i.pow(exp)
    }.sum
  }

  (one to 10).map(idempotentUnaryOps).foreach(println)

  // turns out to be https://oeis.org/A000248

}