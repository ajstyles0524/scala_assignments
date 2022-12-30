package algorithms.sorting

// Like Merge Sort, QuickSort is a Divide and Conquer algorithm.
// It picks an element as a pivot and partitions the given array around the picked pivot.
// There are many different versions of quickSort that pick pivot in different ways.
//    Always pick the first element as a pivot.
//    Always pick the last element as a pivot (implemented below)
//    Pick a random element as a pivot.
//    Pick median as the pivot.
//The key process in quickSort is a partition(). The target of partitions is,
// given an array and an element x of an array as the pivot, put x at its correct position in a sorted array and
// put all smaller elements (smaller than x) before x,
// and put all greater elements (greater than x) after x. All this should be done in linear time.


// 5, 2, 7, 1, 3, 8, 4  i = -1 and j = 0
// 2, 5, 7, 1, 3, 8, 4  i =  0 and j = 1
//
object QuickSort {
  private def partition(arr: Array[Int], low: Int, high: Int):Int = {
    val pivot = arr(high)
    var i = low -1
    for( j <- low until high){
      if(arr(j) <= pivot){
        i += 1
        val temp = arr(i)
        arr(i) = arr(j)
        arr(j) = temp
      }
    }
    val temp = arr(i+1)
    arr(i+1) = arr(high)
    arr(high) = temp
    i+1
  }
  private def quickSort(arr:Array[Int], low: Int, high:Int): Array[Int] = {
    if(low < high) {
      val pi = partition(arr, low, high)
      quickSort(arr, low, pi - 1)
      quickSort(arr, pi + 1, high)
    }
    else {
      arr
    }
  }

  private def quickSortForArray(array: Array[Int]): Array[Int] = {
    if (array.length <= 1) {
      array
    } else {
      val pivot = array(array.length / 2)
      val left = array.filter(_ < pivot)
      val middle = array.filter(_ == pivot)
      val right = array.filter(_ > pivot)
      quickSortForArray(left) ++ middle ++ quickSortForArray(right)
    }
  }

  private def quickSortForList(list: List[Int]): List[Int] = {
    if (list.length <= 1) {
      list
    } else {
      val pivot = list(list.length / 2)
      quickSortForList(list.filter(_ < pivot)) ::: list.filter(_ == pivot) ::: quickSortForList(list.filter(_ > pivot))
    }
  }


  def main(args: Array[String]): Unit = {
    val unsortedArray = Array(5, 2, 7, 1, 3, 8, 4)
    println(unsortedArray.mkString(" "))
    println(quickSort(unsortedArray,0,unsortedArray.length-1).mkString(" "))
    println(quickSortForArray(unsortedArray).mkString(" "))
    val list = List(5,2,7,1,3,8,4)
    println(quickSortForList(list))
  }
}
