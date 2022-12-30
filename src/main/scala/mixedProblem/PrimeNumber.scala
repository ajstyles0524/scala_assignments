package mixedProblem
import scala.annotation.tailrec

object PrimeNumber
{
  private def checkPrime(num: Int): Boolean ={
    @tailrec
    def checkPrimeHelper(num: Int, i: Int = 2): Boolean = {
      if (num <= 2) {
        if (num == 2) true
        else false
      }
      if (num % i == 0)
        return false
      if (i * i > num)
        return true
      checkPrimeHelper(num, i + 1)
    }
    checkPrimeHelper(num,2)
  }

  def main(args: Array[String]): Unit = {
    val num = scala.io.StdIn.readInt()
    println(checkPrime(num))
  }
}
