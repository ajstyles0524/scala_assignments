package algorithms.sorting
import scala.annotation.tailrec

// Bubble Sort is the simplest sorting algorithm that works by repeatedly
// swapping the adjacent elements if they are in the wrong order.

object BubbleSort {

  // arr = 9  6  3   10  -1  2  5
  // 6  3  9   -1   2  5  10 step - 1
  // 3  6  -1   2   5  9 10  step - 2
  // 3  -1  2   5   6  9 10  step - 3
  // -1  2  3   5   6  9 10  step - 4
  @tailrec
  private def bubbleSort(arr:Array[Int]): Unit = {
    if(arr.isEmpty) println("Empty array found")
    else {
      var didSwap = false
      for (i <- 0 until arr.length - 1)
        if (arr(i) > arr(i+1)) {
          val temp = arr(i)
          arr(i) = arr(i + 1)
          arr(i + 1) = temp
          didSwap = true
        }
      // Repeat until we don't have anymore swaps
      if (didSwap)
        bubbleSort(arr)
      else
        println(arr.mkString(" "))
    }
  }


  def main(args: Array[String]): Unit = {
    val arr = Array(-1,100,9,3,-2,3,0,8)
    bubbleSort(arr)
  }

}
