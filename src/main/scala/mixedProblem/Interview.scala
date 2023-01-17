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

  def main(args: Array[String]): Unit = {
    val ls = List(1,2,3,4,5)
    println(sumOfList(ls))
    println(sumOf(10))
    println(reverse("Anand Jaiswal"))
    val str = "Anand JAISWAL"
    reverseRec(str,0,str.length)
  }
}
