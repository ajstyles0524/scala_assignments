package dataStructure.Arrays

object Median extends App{
  private def median1(array1: Array[Int], array2: Array[Int]): Double = {
    val size1 = array1.size
    val size2 = array2.size
    val combined = new Array[Int](size1 + size2)
    var i = 0
    var j = 0
    var k = 0
    while (i < size1 && j < size2) {
      if (array1(i) < array2(j)) {
        combined(k) = array1(i)
        i += 1
      } else {
        combined(k) = array2(j)
        j += 1
      }
      k += 1
    }
    while (i < size1) {
      combined(k) = array1(i)
      i += 1
      k += 1
    }
    while (j < size2) {
      combined(k) = array2(j)
      j += 1
      k += 1
    }
    val size = combined.size
    if (size % 2 == 0) {
      // If the combined array has an even number of elements,
      // return the average of the two middle elements
      (combined(size / 2 - 1) + combined(size / 2)) / 2.0
    } else {
      // If the combined array has an odd number of elements,
      // return the middle element
      combined(size / 2).toDouble
    }
  }

  private def median2(array1: Array[Int], array2: Array[Int]): Double = {
    val combined = (array1 ++ array2).sorted
    val size = combined.size
    if (size % 2 == 0) {
      (combined(size / 2 - 1) + combined(size / 2)) / 2.0
    } else {
      combined(size / 2).toDouble
    }
  }

  private val array1 = Array(1, 3, 5, 7)
  private val array2 = Array(2, 4, 6)
  println(median1(array1, array2))
  println(median2(array1, array2))
}
