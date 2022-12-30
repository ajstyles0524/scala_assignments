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
    for (i <- 0 until  arr.length) {
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


   private def selectionSortArray(array: Array[Int]): Array[Int] = {
    @tailrec
    def inner(array: Array[Int], sortedArray: Array[Int]): Array[Int] = {
      if (array.isEmpty) sortedArray
      else {
        val min = findMinArray(array)
        inner(array.filter(_ != min.get), sortedArray :+ min.get)
      }
    }

    inner(array, Array.empty[Int])
  }

  private def findMinArray(array: Array[Int]): Option[Int] = {
    @tailrec
    def inner(array: Array[Int], min: Option[Int]): Option[Int] = {
      if (array.isEmpty) min
      else {
        if (min.isEmpty) inner(array.tail, Some(array.head))
        else if (min.get > array.head) inner(array.tail, Some(array.head))
        else inner(array.tail, min)
      }
    }
    inner(array, None)
  }


  def main(args: Array[String]): Unit = {
    val array = Array(5, 2, 4, 6, 1, 3)
    selectionSort(array)
    val list = List(5,2,4,6,1,3,-2,-1,0)
    println(selectionSortForList(list,List.empty[Int]))
    val arr = Array(5, 2, 4, 6, 1, 3)
    println(selectionSortArray(arr).mkString(" "))
  }
}
