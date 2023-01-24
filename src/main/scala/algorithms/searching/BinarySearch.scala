package algorithms.searching

import scala.annotation.tailrec

object BinarySearch {
  private def binarySearch(list: List[Int], target: Int): Any = {
    if(list.isEmpty) return "Empty list found"
    @tailrec
    def binarySearchHelper(list: List[Int], target:Int, low: Int, high: Int): Any = {
      if(low > high) "Element is not Present"
      else {
        val middle = low + (high - low) / 2
        if(list(middle) < target) binarySearchHelper(list, target, middle+1, high)
        else if(list(middle) > target) binarySearchHelper(list, target, low, middle-1)
        else middle
      }
    }
    binarySearchHelper(list,target,low=0, high = list.length-1)
  }


  def main(args: Array[String]): Unit = {
    val list = List(1,2,3,5,6,8,10)
    val value = 6
    println(binarySearch(list,value))
  }
}
