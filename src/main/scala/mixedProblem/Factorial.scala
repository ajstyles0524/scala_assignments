package mixedProblem

object Factorial {
  def main(args: Array[String]): Unit ={
    val num = scala.io.StdIn.readInt()
    println(s"Factorial of $num is ${factorial(num)}")
  }
  private def factorial(num: Int): Int = {
      num match {
        case 0 => 1
        case _ => num * factorial(num - 1)
      }
    }
}
