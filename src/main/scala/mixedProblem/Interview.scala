package mixedProblem

import scala.annotation.tailrec

object Interview {
  private def sumOfList(list: List[Int]): Int ={
    if(list.isEmpty) 0
    else if(list.length < 3) list.sum
    else list.takeRight(3).sum
  }


  private def sumOf(num: Int): Int ={
    if(num == 1) 1
    else num + sumOf(num-1)
  }

  def reverse(str: String): String ={
    var reversed = ""
    for(i <- 0 until str.length){
      val value = str.charAt(i)
      reversed = value + reversed
    }
    reversed
  }

   def reverseRec(str: String, index: Int, n: Int): Unit ={
     if(index == n) return
     else{
       val temp = str(index)
       reverseRec(str,index+1,n)
       print(temp)
     }
   }


  private def lengthOfList(lis: List[Int]): Int = {
    @tailrec
    def lengthHelper(lis: List[Int], acc: Int): Int = {
      if (lis.isEmpty) acc
      else lengthHelper(lis.tail, acc + 1)
    }

    lengthHelper(lis, 0)
  }


  //

  def main(args: Array[String]): Unit = {
    val a = scala.io.StdIn.readInt()
    val ls = List(1,2,3,4,5)
    println(sumOfList(ls))
    println(sumOf(10))
    println(reverse("Anand Jaiswal"))

    val str = "Anand JAISWAL"
    //reverseRec(str,0,str.length)

    println()
    println()
    println()
    val lis = List(1, 2, 3)
    println(lengthOfList(lis))
    //val str = "String example at include.com"
    //val subStr = str.substring(18, 29)
    //printf("substring: '%s'\n", subStr)
  }
}
