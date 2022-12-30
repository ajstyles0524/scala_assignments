package algorithms.sorting

// Insertion sort is a simple sorting algorithm that works similar to the way you sort playing cards in your hands.
// The array is virtually split into a sorted and an unsorted part.
// Values from the unsorted part are picked and placed at the correct position in the sorted part.
object InsertionSort {

  // arr = 12  11  13  5  6
  // 11  12  13  5  6
  // 11  12  5   13 6
  // 11  5  12  13  6
  // 5   11 12  13  6
  // 5   11  12   6 13
  // 5   11  6   12 13
  // 5    6  11  12 13
  private def insertionSort(array: Array[Int]): Unit= {
    if(array.isEmpty) println("Empty array found ")
    for (i <- 1 until array.length) {
      val key = array(i)
      var j = i - 1
      while (j >= 0 && array(j) > key) {
        array(j + 1) = array(j)
        j -= 1
      }
      array(j + 1) = key
    }
    println(array.mkString(" "))
  }


  // 3 2 6 9 1 0
  // insert(3,insertionSortForList(2,6,9,1,0)
  // insert(3,insert(2,insertionSortForList(6,9,1,0)))
  // insert(3,insert(2,insert(6,insertionSortForList(9,1,0))))
  // insert(3,insert(2,insert(6,insert(9,insertionSortForList(1,0)))))
  // insert(3,insert(2,insert(6,insert(9,insert(1,insertionSortForList(0))))))
  // insert(3,insert(2,insert(6,insert(9,insert(1,insert(0,insertionSortForList()))))))
  // now case Nil is encountered in list match, now coming to solve insert function one by one
  // insert(3,insert(2,insert(6,insert(9,insert(1,insert(0,Nil)))))
  // insert(3,insert(2,insert(6,insert(9,insert(1,0,Nil))))
  // insert(3,insert(2,insert(6,insert(9,insert(1,0,Nil))))
  // insert(3,insert(2,insert(6,insert(9,0,insert(1,Nil))
  // insert(3,insert(2,insert(6,insert(9,(0,1,Nil)))
  //
  //
  //
  //
  //
  // (0,1,2,3,6,9)
  private def insertionSortForList(list: List[Int]): List[Int] = {
    def insert(x: Int, xs: List[Int]): List[Int] = xs match {
      case Nil => x :: Nil
      case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
    }
    list match {
      case Nil => Nil
      case x :: xs => insert(x, insertionSortForList(xs))
    }
  }


  private def insertionSortUsingRec(array: Array[Int]): Array[Int] = {
    def insert(x: Int, xs: List[Int]): List[Int] = xs match {
      case Nil => x :: Nil
      case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
    }
    def sort(xs: List[Int]): List[Int] = xs match {
      case Nil => Nil
      case y :: ys => insert(y, sort(ys))
    }
    sort(array.toList).toArray
  }

  def main(args: Array[String]): Unit = {
    val array = Array(5, 2, 4, 6, 1, 3)
    insertionSort(array)

    val list = List(5, 2, 4, 6, 1, 3)
    val sortedList = insertionSortForList(list)
    println(sortedList.mkString(", "))

    val arr = Array(5, 2, 4, 6, 1, 3)
    val sortedArray = insertionSortUsingRec(arr)
    println(sortedArray.mkString(", "))
  }
}
