package mixedProblem

object calculator {
  def add(a:Int,b:Int):Int={
    a+b
  }
  private def sub(a:Int, b:Int):Int={
    a-b
  }
  private def multiplication(a:Int, b:Int):Int={
    a*b
  }
  private def division(a:Int, b:Int):Int={
    a/b
  }
  private def abs(a:Int, b:Int):Int={
    val c:Int=a-b
    c.abs
  }
  private def remainder(a:Int, b:Int):Int={
    a%b
  }

  // main function
  def main(args: Array[String]): Unit = {
    val a: Int = scala.io.StdIn.readInt()
    val b: Int = scala.io.StdIn.readInt()
    println(add(a, b))
    println(sub(a, b))
    println(multiplication(a, b))
    println(division(a, b))
    println(abs(a, b))
    println(remainder(a, b))
  }
}
