package mixedProblem

object Even_Odd {
  def main(args: Array[String]): Unit ={

    val num = scala.io.StdIn.readInt()
    if(evenOdd(num)== 1) println(s"$num is Even")
    else println(s"$num is Odd")
  }
  private def evenOdd(num: Int):Int={
    if((num/2)*2 == num) 1
    else 0
  }
}
