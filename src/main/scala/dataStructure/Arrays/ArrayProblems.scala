package dataStructure.Arrays

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.control.Breaks.{break, breakable}

object ArrayProblems extends App{

  private def average(nums: Array[Int]): Unit ={
    println("Original Array elements:")
    for (x <- nums) {
      print(s"${x} ")
    }
    var total = 0.0
    for (i <- nums.indices) {
      total += nums(i)
    }
    println(s"\nAverage value of the array elements is: ${total / nums.length}");
  }

  private def averageHOF(nums: Array[Int]): Double={
    val sum = nums.foldLeft(0)((acc,i) => acc+i)
    val average = sum.toDouble / nums.length
    average
  }

  @tailrec
  private def reverseArray(arr: Array[Int], index: Int = 0): Array[Int] = {
    if (index == arr.length / 2) arr
    else {
      val temp = arr(index)
      arr(index) = arr(arr.length - 1 - index)
      arr(arr.length - 1 - index) = temp
      reverseArray(arr, index + 1)
    }
  }

  private def reverseArrayHOF(arr: Array[Int]): Array[Int] ={
    arr.foldLeft(Array[Int]())((res, curr) => curr +: res)
  }


  private def removeDuplicates(arr:Array[Int],n: Int): Int ={
    if(n==0 || n== 1) return n
    var j = 0
    for(i <- 0 until n-1){
      if(arr(i) != arr(i+1)){
        arr(j) = arr(i)
        j += 1
      }
    }
    arr(j) = arr(n-1)
    j += 1
    j
  }

  def removeDuplicates(arr: Array[Int]): Array[Int] =
    arr.foldLeft(Array[Int]())((acc, x) => if(!acc.contains(x)) acc :+ x else acc)



  private def printDuplicates(arr: Array[Int]): List[Int] ={
      val set = mutable.HashSet[Int]()
      val duplicateElements = for {
        element <- arr
        if !set.add(element)
      } yield element
      duplicateElements.toList
    }


  private def printDuplicate(arr: Array[Int]): Unit ={
    arr.groupBy(identity).filter(_._2.length > 1).keys.foreach(println)
  }

  def rearrangeEvenOdd(arr: Array[Int]): Unit ={
    var evenArr = List[Int]()
    var oddArr = List[Int]()

    for (i <- arr.indices) {
      if (arr(i) % 2 == 0) {
        evenArr ::= arr(i)
      } else {
        oddArr ::= arr(i)
      }
    }

    val result = evenArr.reverse ++ oddArr.reverse
    println(result.mkString(","))
  }

  private def rearrangeEvenOddHOF(arr: Array[Int]): Unit ={
    val (evens, odds) = arr.partition(_ % 2 == 0)
    val result = evens ++ odds
    println(result.mkString(","))
  }

  private def maxProduct(arr: Array[Int]): Unit ={
    var max1 = Int.MinValue
    var max2 = Int.MinValue
    for (i <- arr) {
      if (i > max1) {
        max2 = max1
        max1 = i
      } else if (i > max2 && i != max1) {
        max2 = i
      }
    }
    println(max1 * max2)
  }

  private def maxProductHOF(arr: Array[Int]): Unit ={
    val maxProduct = arr.sorted.takeRight(2).product
    println(maxProduct)
  }

  private def maxDiff(arr: Array[Int]): Unit ={
    var maxDiff = Int.MinValue
    var min = Int.MaxValue
    for (i <- arr) {
      min = math.min(min, i)
      maxDiff = math.max(maxDiff, i - min)
    }
    println(maxDiff)
  }


  private def maxTwo(arr: Array[Int]): Unit ={
    var firstMax = Int.MinValue
    var secondMax = Int.MinValue
    for (i <- arr) {
      if (i > firstMax) {
        secondMax = firstMax
        firstMax = i
      } else if (i > secondMax) {
        secondMax = i
      }
    }
    println(s"First Maximum Element: $firstMax")
    println(s"Second Maximum Element: $secondMax")
  }

  private def maxTwoHOF(arr:Array[Int]): Unit ={
    val firstMax = arr.max
    val secondMax = arr.filter(_ != firstMax).max
    println(firstMax)
    println(secondMax)
  }


  private def isSorted(nums: Array[Int]): Boolean = {
    for (i <- 1 until nums.length) {
      if (nums(i - 1) > nums(i)) return false
    }
    true
  }

  private def isSortedHOF(nums: Array[Int]): Boolean = {
    // nums.zip(nums.tail).forall { case (a, b) => a <= b }
    nums.zip(nums.tail).forall(c => c._1 <= c._2)
  }


  def removeValue(nums: Array[Int], value: Int): Array[Int] = {
    nums.filter(_!= value)
  }

  private def leftRotate(arr: Array[Int], d: Int): Array[Int] ={
    var p = 1
    while(p <= d){
      val last = arr(0)
      for(i <- 0 until arr.length-1){
        arr(i) = arr(i+1)
      }
      arr(arr.length-1) = last
      p += 1
    }
    arr
  }


  private def rightRotate(arr: Array[Int], d: Int): Array[Int] ={
    var p = 1
    while(p <= d){
      val first = arr(arr.length-1)
      for(i <- arr.length-1 until 0 by -1){
        arr(i) = arr(i-1)
      }
      arr(0) = first
      p += 1
    }
    arr
  }


  // working on it

  // 1 2 3 4 5
  // 4 5 1 2 3
  // 3 4 5 1 2
  private def rotateLeftHOF(arr: Array[Int], n:Int): Array[Int] ={
    arr.foldRight(List[Int]()){(elem, acc) => if(acc.size == n) acc else elem::acc }.toArray
  }
  def rotateRightHOF(arr: Array[Int],d:Int): Array[Int]= {
    arr.foldLeft(Array[Int]())((acc, i) => if (acc.length < d) acc :+ i else i +: acc)
  }


  private def moveZeroes(nums: Array[Int]): Array[Int] = {
    var nonZeroIndex = 0
    for (i <- nums.indices) {
      if (nums(i) != 0) {
        val temp = nums(nonZeroIndex)
        nums(nonZeroIndex) = nums(i)
        nums(i) = temp
        nonZeroIndex += 1
      }
    }
    nums
  }


  def unionOfArray(arr1: Array[Int], arr2: Array[Int]): Unit ={
    val unionArray = ArrayBuffer[Int]()
    for (elem <- arr1) {
      unionArray += elem
    }
    for (elem <- arr2) {
      if (!unionArray.contains(elem)) {
        unionArray += elem
      }
    }
    val result = unionArray.toArray
    println(result.mkString(" "))
  }

  private def union(arr1: Array[Int], arr2: Array[Int]): Unit = {
    val union = (arr1 ++ arr2).foldLeft(List[Int]())((acc, x) =>  if (!acc.contains(x))  x :: acc else acc ).reverse.toArray
    println(union.mkString(" "))
  }

  def unionTwoPointer(arr1: Array[Int],arr2: Array[Int]): Unit ={
    val array1 = arr1
    val array2 = arr2
    var i = 0
    var j = 0
    while (i < array1.length && j < array2.length) {
      if (array1(i) < array2(j)) {
        print(array1(i) + " ")
        i += 1
      } else if (array1(i) > array2(j)) {
        print(array2(j) + " ")
        j += 1
      } else {
        print(array1(i) + " ")
        i += 1
        j += 1
      }
    }
    while (i < array1.length) {
      print(array1(i) + " ")
      i += 1
    }
    while (j < array2.length) {
      print(array2(j) + " ")
      j += 1
    }
  }

  def intersectionOfArray(arr1: Array[Int], arr2: Array[Int]): Unit ={
    val array1 = arr1
    val array2 = arr2
    var i = 0
    var j = 0
    val intersection = ArrayBuffer[Int]()
    while (i < array1.length && j < array2.length) {
      if (array1(i) < array2(j)) {
        i += 1
      } else if (array1(i) > array2(j)) {
        j += 1
      } else {
        intersection += array1(i)
        i += 1
        j += 1
      }
    }
    val result = intersection.toArray
  }


  def intersection(arr1: Array[Int], arr2: Array[Int]): Unit = {
    val array1 = arr1
    val array2 = arr2
    val intersection = ArrayBuffer[Int]()
    for (elem1 <- array1) {
      for (elem2 <- array2) {
        if (elem1 == elem2) intersection += elem1
      }
    }
    val result = intersection.toArray
  }

  def intersectionHOF(arr1: Array[Int], arr2: Array[Int]): Unit = {
    val array1 = arr1
    val array2 = arr2
    val intersection = array1.foldLeft(List[Int]()) {
      case (acc, x) if array2.contains(x) => x :: acc
      case (acc, _) => acc
    }.toArray
  }

  def findMissingNumber(nums: Array[Int]): Int = {
    var expectedSum = nums.length * (nums.length + 1) / 2
    var actualSum = 0
    for (i <- nums.indices) {
      actualSum += nums(i)
    }
    expectedSum - actualSum

   // val n = nums.length
   // val expectedSum = (n * (n + 1)) / 2
   // nums.foldLeft(expectedSum)((acc, x) => acc - x)
  }


  def maxConsecutiveOnes(nums: Array[Int]): Int = {
    var max = 0
    var current = 0
    for (num <- nums) {
      if (num == 1) {
        current += 1
        max = math.max(max, current)
      } else {
        current = 0
      }
    }
    max
    // nums.foldLeft((0, 0))((acc, x) => if (x == 1) (acc._1 + 1, math.max(acc._1 + 1, acc._2)) else (0, acc._2))._2
  }


  private def subArraySum(arr: Array[Int], s: Int): List[Int] = {
    val res = ListBuffer[Int]()
    var st = 0
    var sum = 0
    for(i <- arr.indices){
      sum += arr(i)
      while(sum > s){
        sum -= arr(st)
        st += 1
      }
      if(sum ==s && sum!= 0){
        res += st+1
        res += i+1
        return  res.toList
      }
    }
    res += -1
    res.toList
  }
  // 1 2 3 4 5


  def subarraySum(nums: Array[Int], k: Int): Int = {
    var count = 0
    var sum = 0
    val map = scala.collection.mutable.Map[Int, Int]()
    map(0) = 1
    for (i <- nums.indices) {
      sum += nums(i)
      if (map.contains(sum - k))
        count += map(sum - k)
      map(sum) = map.getOrElse(sum, 0) + 1
    }
    count
  }


  def findZeroSumSubarray(arr: Array[Int]): (Int, Int) = {
    val prefixSum = Array.ofDim[Int](arr.length)
    prefixSum(0) = arr(0)
    for (i <- 1 until arr.length) {
      prefixSum(i) = prefixSum(i - 1) + arr(i)
    }
    val map = scala.collection.mutable.HashMap[Int, Int]()
    for (i <- arr.indices) {
      if (map.contains(prefixSum(i))) {
        return (map(prefixSum(i)) + 1, i)
      }
      map += (prefixSum(i) -> i)
    }
    (-1, -1)
  }


  def findFrequency(A: Array[Int]): Unit = {
    //val frequency = arr.groupBy(identity).mapValues(_.size)
    //val frec = A.foldLeft(Map[Int, Int]())((map, x) => map + (x -> (map.getOrElse(x, 0) + 1)))
    val arr = Array(1, 2, 3, 2, 1, 3, 4, 5)
    val frequency = scala.collection.mutable.Map[Int, Int]()
    for (i <- arr.indices) {
      if (frequency.contains(arr(i))) {
        frequency(arr(i)) += 1
      } else {
        frequency(arr(i)) = 1
      }
    }
    println(frequency)
  }



    private def printfrequency(arr: Array[Int], n: Int): Unit = {
      for (j <- 0 until n) {
        arr(j) = arr(j) - 1
      }
      for (i <- 0 until n) {
        arr(arr(i) % n) = arr(arr(i) % n) + n
      }
      for (i <- 0 until n) {
        System.out.println(i + 1 + "->" + arr(i) / n)
      }
    }


  def kthSmallest(arr: Array[Int], k: Int): Int = {
    val sortedArray = arr.sorted
    return sortedArray(k - 1)
  }


  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val map = scala.collection.mutable.Map[Int, Int]()
    for (i <- nums.indices) {
      val complement = target - nums(i)
      if (map.contains(complement)) {
        return Array(map(complement), i)
      }
      map(nums(i)) = i
    }
    throw new IllegalArgumentException("No two sum solution")
  }

  val raw = Array(1,2,3,4,5)
  println(twoSum(raw,7).mkString(" "))


  def indicesOfSum(arr: Array[Int], target: Int): (Int, Int) = {
    arr.zipWithIndex.find { case (elem, index) =>
      val complement = target - elem
      val complementIndex = arr.indexOf(complement)
      complementIndex != -1 && complementIndex != index
    } match {
      case Some((elem, index)) => (index, arr.indexOf(target - elem))
      case None => (-1, -1)
    }
  }


  def find3Numbers(A: Array[Int], arr_size: Int, sum: Int): Boolean = {
    for (i <- 0 until arr_size - 2) {
      val s = scala.collection.mutable.HashSet[Int]()
      val curr_sum = sum - A(i)
      for (j <- i + 1 until arr_size) {
        if (s.contains(curr_sum - A(j))) {
          System.out.print("Triplet is " + A(i) + ", " + A(j) + ", " + (curr_sum - A(j)))
          return true
        }
        s.add(A(j))
      }
    }
    false
  }

  def find3Number(A: Array[Int], arr_size: Int, sum: Int): Boolean = {
    var l = 0
    var r = 0
   A.sorted
    for (i <- 0 until arr_size - 2) {
      l = i + 1
      r = arr_size - 1
      while (l < r) if (A(i) + A(l) + A(r) == sum) {
        System.out.print("Triplet is " + A(i) + ", " + A(l) + ", " + A(r))
        return true
      }
      else if (A(i) + A(l) + A(r) < sum) l += 1
      else r -= 1 // A[i] + A[l] + A[r] > sum
    }
    false
  }


  def majorityElement(nums: Array[Int]): Int = {
    val map = scala.collection.mutable.Map[Int, Int]()
    for (i <- nums.indices) {
      if (map.contains(nums(i))) {
        map(nums(i)) += 1
      } else {
        map += (nums(i) -> 1)
      }
      if (map(nums(i)) > nums.length / 2) {
        return nums(i)
      }
    }
    throw new IllegalArgumentException("No majority element")
  }

  def sortBinaryArray(nums: Array[Int]): Unit = {
    var low = 0
    var high = nums.length - 1

    while (low < high) {
      if (nums(low) == 1) {
        val temp = nums(low)
        nums(low) = nums(high)
        nums(high) = temp
        high -= 1
      } else {
        low += 1
      }
    }
  }


  def sortColors(nums: Array[Int]): Unit = {
    var low = 0
    var mid = 0
    var high = nums.length - 1
    while (mid <= high) {
      nums(mid) match {
        case 0 =>
          val temp = nums(low)
          nums(low) = nums(mid)
          nums(mid) = temp
          low += 1
          mid += 1
        case 1 =>
          mid += 1
        case 2 =>
          val temp = nums(high)
          nums(high) = nums(mid)
          nums(mid) = temp
          high -= 1
      }
    }
  }

  def findFirstRepeating(nums: Array[Int]): Int = {
    val set = new collection.mutable.HashSet[Int]()
    for (i <- nums.indices) {
      if (set.contains(nums(i))) {
        return nums(i)
      }
      set += nums(i)
    }
    -1
  }


  def firstRepeating(arr: Array[Int]): Int = {
    arr.foldLeft(Array.empty[Int]) { (seen, element) => if (seen.contains(element)) return element else seen :+ element}
    -1
  }


  val war = Array(1,2,3,5,2)
  println(firstRepeating(war))

  def rearrangeArray(nums: Array[Int]): Array[Int] = {
    val pos = nums.filter(_ > 0)
    val neg = nums.filter(_ <= 0)
    val res = new Array[Int](nums.length)
    for (i <- pos.indices) {
      res(i * 2) = pos(i)
      res(i * 2 + 1) = neg(i)
    }
    res
  }
  private def maxProductArray(nums: Array[Int]): Int = {
    var maxProduct = nums(0)
    var currMax = nums(0)
    var currMin = nums(0)
    for (i <- 1 until nums.length) {
      if (nums(i) < 0) {
        val temp = currMax
        currMax = currMin
        currMin = temp
      }
      currMax = math.max(nums(i), currMax * nums(i))
      currMin = math.min(nums(i), currMin * nums(i))
      maxProduct = math.max(maxProduct, currMax)
    }
     maxProduct
  }


  def maxSubArray(nums: Array[Int]): Int = {
    var maxSum = nums(0)
    var currSum = nums(0)
    for (i <- 1 until nums.length) {
      currSum = math.max(nums(i), currSum + nums(i))
      maxSum = math.max(maxSum, currSum)
    }
    maxSum
  }

  def containsDuplicate(arr: Array[Int]): Boolean = {
    val set = scala.collection.mutable.HashSet[Int]()
    for (i <- arr) {
      if (set.contains(i)) {
        return true
      }
      set += i
    }
    false
  }

  def containsDuplicates(arr: Array[Int]): Boolean = {
    arr.sorted.sliding(2).exists(x => x(0) == x(1))
  }

  def firstNonRepeating(arr: Array[Int]): Unit ={
    // val firstNonRepeating = arr.groupBy(identity).filter(_._2.length == 1).map(_._1).head
    val counts = new scala.collection.mutable.HashMap[Int, Int]()
    for (i <- arr.indices) {
      if (counts.contains(arr(i))) {
        counts(arr(i)) += 1
      } else {
        counts(arr(i)) = 1
      }
    }
    breakable{
      for (i <- arr.indices) {
        if (counts(arr(i)) == 1) {
          println(arr(i))
          break
        }
      }
    }
  }




  private var nums = Array(1, 2, 3, 4, 5, 6)
  average(nums)
  println(averageHOF(nums))

  println(reverseArray(nums).mkString(" "))
  println(reverseArrayHOF(nums).mkString(" "))

  val arr = Array(1,1,2,3,4,5,2,4,6)
  val index = removeDuplicates(arr.sorted,arr.length)
  println()
  for (i <- 0 until  index) {
    print(arr(i))
  }
  println()
  println(printDuplicates(arr))

  println()
  printDuplicate(arr)

  println()
  rearrangeEvenOdd(arr)
  rearrangeEvenOddHOF(arr)

  println()
  val a = Array(1, -10, 3, 7, 2, 20, 30)
  //maxProduct(a)
  maxProductHOF(a)

  println()
  maxDiff(a)
  maxTwo(a)
  maxTwoHOF(a)

  println()
  println(isSorted(a))
  println(isSortedHOF(a))
  println(leftRotate(a,1).mkString(" "))
  println(rightRotate(a,1).mkString(" "))

  println()
  println()
  val input = Array(1,2,3,7,5)
  print(subArraySum(input,12))
  println()
  println(findZeroSumSubarray(a))

  println()
  println()
  printfrequency(Array(1,2,3,1,2,5,4,2),8)






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








//  private val array1 = Array(1, 3, 5, 7)
//  private val array2 = Array(2, 4, 6)
//  println(median1(array1, array2))
//  println(median2(array1, array2))
}
