package mixedProblem
import scala.annotation.tailrec

object AddTwoNum {
  def main(args: Array[String]): Unit = {
    val num1 = scala.io.StdIn.readInt()
    val num2 = scala.io.StdIn.readInt()
    println(s"The Sum of $num1 and $num2 is ${add(num1, num2)}")
  }
  private def add(num1: Int, num2: Int): Int = {
    @tailrec def addTwoNum(n1: Int, n2: Int): Int = {
      if(n2 == 0)  n1
      else addTwoNum(n1^n2, (n1&n2)<<1)
    }
    addTwoNum(num1, num2)
  }
}
