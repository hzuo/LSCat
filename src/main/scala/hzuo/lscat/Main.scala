package hzuo.lscat

object Main extends App {

  def solution[T](problem: Int*)(t: T): T = {
    val result = t
    printf("%s\t%s%n", problem.mkString("."), result.toString)
    result
  }

  solution(1, 6) { FunctionCounting.idempotentUnaryOps(3) }
  solution(1, 7) { FunctionCounting.idempotentUnaryOps(2) }
  solution(1, 8) { FunctionCounting.inverses(3, 2) }
  solution(1, 9) { FunctionCounting.inverses(2, 3) }

}