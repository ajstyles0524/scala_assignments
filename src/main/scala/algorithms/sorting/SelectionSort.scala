package algorithms.sorting
import scala.annotation.tailrec

object SelectionSort {

  // The selection sort algorithm sorts an array by repeatedly finding
  // the minimum element from the unsorted part and putting it at the beginning.
  //
  //The algorithm maintains two subarrays in a given array.
  //
  //
  //In every iteration of the selection sort, the minimum element
  //from the unsorted subarray is picked and moved to the sorted subarray.

  //arr =  64 25 12 22 11
  // 11 25 12 22 64
  // 11 12 25 22 64
  // 11 12 22 25 64

  private def selectionSort(arr: Array[Int]): Unit = {
    for (i <- arr.indices) {
      var minIndex = i
      for (j <- i + 1 until arr.length) {
        if (arr(j) < arr(minIndex)) {
          minIndex = j
        }
      }
      val temp = arr(i)
      arr(i) = arr(minIndex)
      arr(minIndex) = temp
    }
    println(arr.mkString(" "))
  }


  @tailrec
  private def findMinForList(list: List[Int], min: Int): Int = {
    list match {
      case Nil => min
      case first :: _ =>
        if (first < min) findMinForList(list.tail, first)
        else findMinForList(list.tail, min)
    }
  }

  @tailrec
  private def selectionSortForList(list: List[Int], sortedList: List[Int]): List[Int] = {
    if (list.isEmpty) sortedList.reverse
    else {
      val minimum = findMinForList(list, list.head)
      selectionSortForList(list.filter(x => x != minimum), minimum :: sortedList)
    }
  }




  def main(args: Array[String]): Unit = {
    val array = Array(5, 2, 4, 6, 1, 3)
    selectionSort(array)
    val list = List(5,2,4,6,1,3,-2,-1,0)
    println(selectionSortForList(list,List.empty[Int]))

  }
}
