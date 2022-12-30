package mixedProblem
import scala.annotation.tailrec


object BigFactorial {
  def main(args: Array[String]): Unit = {
    val num = scala.io.StdIn.readInt()
    println(s"Factorial of $num is ${factorial(num)}")
  }
  private def factorial(num: Int): BigInt = {
    @tailrec def factHelper(num: Int, accumulator: BigInt): BigInt = {
      if (num <= 1) accumulator
      else factHelper(num - 1, num * accumulator)
    }
    factHelper(num, 1)
  }
}
