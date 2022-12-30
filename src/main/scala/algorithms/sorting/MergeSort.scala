package algorithms.sorting

// The Merge Sort algorithm is a sorting algorithm that is based on the Divide and Conquer paradigm.
// In this algorithm, the array is initially divided into two equal halves and then
// they are combined in a sorted manner.

// Think of it as a recursive algorithm continuously splits the array in half until
// it cannot be further divided. This means that if the array becomes empty or has only one element left,
// the dividing will stop, i.e. it is the base case to stop the recursion.

object MergeSort {
  // 5 4 0 9 1
  private def mergeSort(array: Array[Int]): Array[Int] = {
    if (array.length <= 1) return array
    val middle = array.length / 2
    val left = mergeSort(array.slice(0, middle))
    val right = mergeSort(array.slice(middle, array.length))
    merge(left, right)
  }
  private def merge(left: Array[Int], right: Array[Int]): Array[Int] = {
    var result = Array.empty[Int]
    var leftIndex = 0
    var rightIndex = 0
    while (leftIndex < left.length && rightIndex < right.length) {
      if (left(leftIndex) < right(rightIndex)) {
        result = result :+ left(leftIndex)
        leftIndex += 1
      } else {
        result = result :+ right(rightIndex)
        rightIndex += 1
      }
    }
    while (leftIndex < left.length) {
      result = result :+ left(leftIndex)
      leftIndex += 1
    }
    while (rightIndex < right.length) {
      result = result :+ right(rightIndex)
      rightIndex += 1
    }
    result
  }

  private def mergeSortForList(list: List[Int]): List[Int] = {
    if(list.length <= 1) return list
    val mid = list.length/2
    val left = mergeSortForList(list.slice(0,mid))
    val right = mergeSortForList(list.slice(mid,list.length))
    mergeList(left, right)
  }

  private def mergeList(left: List[Int], right: List[Int]): List[Int] = {
    (left, right) match {
      case (Nil, _) => right
      case (_, Nil) => left
      case (x :: xs, y :: ys) =>
        if (x < y) x :: mergeList(xs, right)
        else y :: mergeList(left, ys)
    }
  }

  def main(args: Array[String]): Unit = {
    val array = Array(5, 4, 3, 2, 1)
    val sortedArray = mergeSort(array)
    println(sortedArray.mkString(", ")) // prints "1, 2, 3, 4, 5"

    val list = List(5, 4, 3, 2, 1)
    val sortedList = mergeSortForList(list)
    println(sortedList)
  }
}
