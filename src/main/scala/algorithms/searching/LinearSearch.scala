package algorithms.searching
import scala.annotation.tailrec

object LinearSearch {
  @tailrec
  private def linearSearch(list: List[Int], target: Int, index: Int = 0): Any ={
    if(list.isEmpty) "Empty list found"
    else if(index >= list.length) "Element is not Present"
    else if(list(index) == target) index
    else linearSearch(list, target, index+1)
  }

  @tailrec
  private def linearSearchForArray(arr: Array[Int], target: Int, index: Int = 0): Any = {
    if(arr.isEmpty) "Empty array found"
    else if (index >= arr.length) "Element is not Present"
    else if (arr(index) == target) index
    else linearSearchForArray(arr, target, index + 1)
  }

  def main(args: Array[String]): Unit = {
    val list = List(1,2,3,4,5)
    val target = 5
    println(linearSearch(list,target))

    val arr = Array(6,5,9,7,0,-3,8)
    val value = 6
    println(linearSearchForArray(arr,value))

  }
}
