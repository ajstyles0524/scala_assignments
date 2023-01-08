package mixedProblem

object Interview {
  def sumOfList(list: List[Int]): Int ={
    if(list.isEmpty) 0
    else if(list.length < 3) list.sum
    else list.takeRight(3).sum

  }
  def main(args: Array[String]): Unit = {
    val ls = List(1,2,3,4,5)
    println(sumOfList(ls))
  }
}
