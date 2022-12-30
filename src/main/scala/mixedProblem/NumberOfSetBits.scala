package mixedProblem
import scala.annotation.tailrec

object NumberOfSetBits {
  def main(args: Array[String]): Unit ={
    val num = scala.io.StdIn.readInt()
    println(s"The number of set bits in $num is ${countSetBits(num)}")
  }

  private def countSetBits(num: Int): Int ={
    @tailrec def countSetBitsHelper(num: Int, acc: Int): Int ={
      if(num == 0) acc
      else countSetBitsHelper(num>>1, acc + (num & 1))
    }
    countSetBitsHelper(num,0)
  }
}
