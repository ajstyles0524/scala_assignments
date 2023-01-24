package algorithms.sorting
import scala.annotation.tailrec

// Bubble Sort is the simplest sorting algorithm that works by repeatedly swapping the adjacent elements if they are in the wrong order.

object BubbleSort {

  // arr = 9  6  3   10  -1  2  5
  // 6  3  9   -1   2  5  10 step - 1
  // 3  6  -1   2   5  9 10  step - 2
  // 3  -1  2   5   6  9 10  step - 3
  // -1  2  3   5   6  9 10  step - 4


  private def bubbleSortArray(arr: Array[Int]): Array[Int] = {
    for (i <- 0 until arr.length-1) {
      for (j <- 0 until arr.length-i-1) {
        if (arr(j) > arr(j + 1)) {
          val tmp = arr(j)
          arr(j) = arr(j + 1)
          arr(j + 1) = tmp
        }
      }
    }
    arr
  }

  // 5 4 3 2 1
  def bubbleSortList(list: List[Int]): List[Int] = {
    for (i <- 0 until list.length - 1) {
      for (j <- 0 until list.length - 1 - i) {
        if (list(j) > list(j + 1)) {
          val tmp = list(j)
          list.updated(j, list(j + 1)).updated(j + 1, tmp)
        }
      }
    }
    list
  }


  private def bubbleSort(list: List[Int]): List[Int] = {
    def swap(list: List[Int], i: Int, j: Int): List[Int] = {
      val tmp = list(i)
      list.updated(i, list(j)).updated(j, tmp)
    }
    @tailrec
    def sort(list: List[Int], n: Int): List[Int] = {
      if (n == 1) list
      else {
        val newList = (0 to n - 2).foldLeft(list) ( (l, i) =>if (l(i) > l(i + 1)) swap(l, i, i + 1) else l)
        sort(newList, n - 1)
      }
    }
    sort(list, list.length)
  }

  def bubbleSortHOF(arr: Array[Int]): Array[Int] = {
    def swap(arr: Array[Int], i: Int, j: Int): Array[Int] = {
      val tmp = arr(i)
      arr(i) = arr(j)
      arr(j) = tmp
      arr
    }
    @tailrec
    def sort(arr: Array[Int], n: Int): Array[Int] = {
      if (n == 1) arr
      else {
        val newArr = (0 to n- 2).foldLeft(arr) { (a, i) => if (a(i) > a(i + 1)) swap(a, i, i + 1) else a}
        sort(newArr, n - 1)
      }
    }
    sort(arr, arr.length)
  }




  def main(args: Array[String]): Unit = {
    val arr = Array(-1,100,9,3,-2,3,0,8)
    val list = List(-1,100,9,3,-2,3,0,8)
    println(bubbleSort(list))


  }

}
