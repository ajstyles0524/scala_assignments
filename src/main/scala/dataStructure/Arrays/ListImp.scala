package dataStructure.Arrays

import scala.annotation.tailrec

object ListImp extends App{

  private def show(f: Int, n: Int): List[Int] = {
    (1 to n).flatMap(i => List.fill(f)(i)).toList
  }

  def show1(f: Int, n: Int): List[Int] = {
    (1 to n).foldLeft(List[Int]())((acc, i) => acc ++ List.fill(f)(i))
  }

  def show2(f: Int, n: Int): List[Int] = {
    (1 to n).foldLeft(List[Int]())((acc, i) => acc ++ (1 to f).map(_ => i))
  }

  private def maxAndMin(arr:Array[Int]): Unit ={
    val max = arr.reduce((a,b) => if(a>b) a else b)
    val min = arr.reduce((a,b) => if(a<b) a else b)
    val (firstMax, secondMax) = arr.foldLeft((Int.MinValue, Int.MinValue)){ (max, x) =>if (x > max._1) (x, max._1) else if (x > max._2) (max._1, x) else max}
    println(max)
    println(min)
    println(s"First max: $firstMax, Second max: $secondMax")
  }

  @tailrec
  def reverse(list: List[Int], acc: List[Int]): List[Int] ={
    if(list == Nil) acc
    else reverse(list.tail, list.head :: acc)
  }

  private def reverseHOF(list: List[Int]): List[Int]={
    list.foldLeft(List[Int]())((acc,i) => if(acc.length == list.length) acc else i :: acc)
  }

  private def flat_List(ls: List[_]): List[Any] = ls match {
    case Nil => Nil
    case (head: List[_]) :: tail => flat_List(head) ::: flat_List(tail)
    case head :: tail => head :: flat_List(tail)
  }

  def rotate(arr: Array[Int], r: Int): Array[Int] ={
    arr.drop(arr.length - r) ++ arr.take(arr.length - r)
  }

  def sum(opt1: Option[Int], opt2: Option[Int]): Option[Int] = {
    (opt1, opt2) match {
      case (Some(x), Some(y)) => Some(x + y)
      case (None, _) | (_, None) => None
    }
  }

  def fill(element: Int)(noOfTimes: Int): List[Int] = {
    List.fill(noOfTimes)(element)
  }

  private def fill1(element: Int)(noOfTimes: Int): List[Int] = {
    (1 to noOfTimes).foldLeft(List[Int]())((acc, _) => element :: acc).reverse
  }

  def removeDuplicates(list: List[Int]): List[Int] ={
    list.foldLeft(List[Int]())((acc,i) => if(acc.contains(i)) acc else i :: acc).reverse
  }

  private def removeDuplicate(list: List[Int]): List[Int] = {
    def go(remaining: List[Int], acc: List[Int]): List[Int] = remaining match {
      case Nil => acc
      case x :: xs if !acc.contains(x) => go(xs, x :: acc)
      case _ :: xs => go(xs, acc)
    }
    go(list, Nil).reverse
  }

  def countWord(text: String, word: String): Int = {
    text.split(" ").count(_ == word)
  }

  def countWord2(text: String, word: String): Int = {
    text.split(" ").foldLeft(0) {
      case (count, w) if w == word => count + 1
      case (count, _) => count
    }
  }


  private def wordCount(str: String): Map[String,Int] ={
    str.split(" ").groupBy(x => x).map(c => (c._1,c._2.length))
  }

  def countWords(sentence: String): Int = {
    sentence.split(" ").foldLeft(0)((acc,_) => acc + 1)
  }

  def count(str: String): Int ={
    str.split(" ").groupBy(x => x).mapValues(_.length).foldLeft(0)((acc,i) => acc + i._2)
  }


  // extending AnyVal is optional
  implicit class RichInt(val value: Int) extends AnyVal {
    def square: Int = value * value
  }


  import java.io.File
  private def countFiles(dir: String): Option[Int] = {
    val file = new File(dir)
    if (file.exists() && file.isDirectory) {
      Some(file.listFiles().length)
    } else {
      None
    }
  }

  private def removeOdd(map: Map[String, Int]): Map[String, Int] = {
    //map.foldLeft(Map[String,Int]())((acc,i) => if(i._2 % 2 == 0) acc + i else acc)
    map.filter { case (_, value) => value % 2 == 0 }
  }

  private def concatList(list1: List[Int], list2: List[Int]): List[Int] ={
    list2.foldLeft(list1)((acc,i) => acc :+ i)
  }

  private def concat(list1: List[Int], list2: List[Int]): List[Int] = {
    list1 match {
      case Nil => list2
      case x :: xs => x :: concat(xs, list2)
    }
  }


  private def concatenate(map1: Map[String, Int], map2: Map[String, Int]): Map[String, Int] = {
    map2.foldLeft(map1)((acc,i) => acc + (i._1 -> (i._2 + acc.getOrElse(i._1,0))))
  }

  def zip(list1: List[Int], list2: List[String]): List[(Int, String)] = {
    val minLength = math.min(list1.length, list2.length)
    (0 until minLength).foldLeft(List[(Int, String)]())((acc, i) => (list1(i), list2(i)) :: acc).reverse
  }

  def merge(list1: List[Int], list2: List[Int]): List[Int] = {
    (list1, list2) match {
      case (Nil, _) => list2
      case (_, Nil) => list1
      case (x :: xs, y :: ys) => if (x < y) x :: merge(xs, list2) else y :: merge(list1, ys)
    }
  }

  private def counter(str: String): List[(String,Int)] ={
     val wordCounts = str.split(" ").groupBy(x => x).map(x => (x._1,x._2.length)).toList  //.sortBy(-_._2)
     wordCounts.sortWith {
      case ((s1, count1), (s2, count2)) => count1 > count2 || (count1 == count2 && s1 < s2)
    }
  }

  // using foldLeft
  private def counters(str: String): List[(String, Int)] = {
    val wordCounts = str.split(" ").foldLeft(Map[String, Int]())((acc, i) => acc + (i -> (1 + acc.getOrElse(i, 0)))).toList
    wordCounts.sortWith {
      case ((s1, count1), (s2, count2)) => count1 > count2 || (count1 == count2 && s1 < s2)
    }
  }


  // implement map and flatmap using foldLeft
  private class MyList(list: List[Int]) {
    def map(f: Int => Int): List[Int] = {
      list.foldLeft(List[Int]())((acc,i) => f(i) :: acc).reverse
      //list.foldRight(List[Int]())((i, acc) => f(i) :: acc )
    }
    def flatMap(f: Int => List[Int]): List[Int] = {
      list.foldLeft(List[Int]())((acc,i) => acc ++ f(i))
    }
  }


  private def composeFunction[A, B, C](g: B => C, f: A => B): A => C = {
    (x: A) => g(f(x))
  }
  private val gun: Double => String = (x: Double) => x.toString
  private val fun: Int => Double = (x: Int) => x.toDouble
  private val composedFunction: Int => String = composeFunction(gun, fun)
  val res: String = composedFunction(5)
  println(res)


  def fuse[A,B](a:Option[A], b:Option[B]):Option[(A,B)] = a.flatMap(aVal => b.map(bVal => (aVal, bVal)))

  def fuse2[A, B](a: Option[A], b: Option[B]): Option[(A, B)] = (a, b) match {
    case (Some(aVal), Some(bVal)) => Some((aVal, bVal))
    case _ => None
  }

  def window(length:Int, list:List[Int]):List[List[Int]] = list.sliding(length).toList

  def window2(length: Int, list: List[Int]): List[List[Int]] = {
    list.foldLeft(List[List[Int]]()) { (acc, _) =>
      acc :+ list.slice(acc.length, acc.length + length)
    }
  }

  def reduce(list: List[Map[String, Int]]): Map[String, Int] = {
    list.reduce((acc, m) => acc ++ m.map { case (k, v) => k -> (v + acc.getOrElse(k, 0))})
  }

  def reduce2(list: List[Map[String, Int]]): Map[String, Int] = {
    list.foldLeft(Map[String, Int]())((acc, ele) => acc ++ ele.map { case (k, v) => k -> (v + acc.getOrElse(k, 0)) })
  }

  private def reduce3(list: List[Map[String, Map[String, Int]]]): Map[String, Map[String, Int]] = {
    list.foldLeft(Map.empty[String, Map[String, Int]]) { (acc, m) =>
      m.foldLeft(acc) { (innerAcc, innerM) =>
        val (key, value) = innerM
        val mergedMap = value.foldLeft(innerAcc.getOrElse(key, Map.empty[String, Int])) { (innerInnerAcc, innerInnerM) =>
          val (innerKey, innerValue) = innerInnerM
          innerInnerAcc + (innerKey -> (innerValue + innerInnerAcc.getOrElse(innerKey, 0)))
        }
        innerAcc + (key -> mergedMap)
      }
    }
  }


  def merge(map1: Map[String, Int], map2: Map[String, Int]): Map[String, Int] = {
    map2.foldLeft(map1)((acc, ele) => acc + (ele._1 -> (ele._2 + acc.getOrElse(ele._1, 0))))
  }

  def merge2(map1: Map[String, Map[String, Int]], map2: Map[String, Map[String, Int]]): Map[String, Map[String, Int]] = {
    map2.foldLeft(map1) { (acc, entry) =>
      val (key, innerMap) = entry
      val mergedInnerMap = innerMap.foldLeft(acc.getOrElse(key, Map.empty[String, Int])) { (innerAcc, innerEntry) =>
        val (innerKey, innerValue) = innerEntry
        innerAcc + (innerKey -> (innerValue + innerAcc.getOrElse(innerKey, 0)))
      }
      acc + (key -> mergedInnerMap)
    }
  }

  private def sortCharacter(str: String): Unit ={
    println(str.groupBy(identity).toList.sortBy(_._2.length)(Ordering.Int.reverse).map(_._2).mkString)
  }

  private def checkPrime(num: Int): Boolean = {
    @tailrec
    def checkPrimeHelper(num: Int, i: Int = 2): Boolean = {
      if (num <= 2) {
        if (num == 2) return true
        else return false
      }
      if (num % i == 0)
        return false
      if (i * i > num)
        return true
      checkPrimeHelper(num, i + 1)
    }
    checkPrimeHelper(num, 2)
  }

  private def primesInRangeHOF(start: Int, end: Int): List[Int] = {
    val numbers = start to end
    numbers.filter(n => (2 until n).forall(x => n % x != 0)).toList
  }
  println(primesInRangeHOF(1, 100))


  private def generateSequence: Seq[String] = {
    (51 to 100).map(x => s"X$x")
  }
  private val sequence = generateSequence.mkString(", ")
  println(sequence)


  def findSmallest(numbers: List[Int]): Int = {
    numbers.reduce((x, y) => if (x < y) x else y)
  }

  private def oddIntegers: List[Int] = {
    (-200 to 200).filter(x => x%2 != 0).toList
  }
  println(oddIntegers)

  def isDivisibleBy2(numbers: List[Int]): List[Boolean] = {
    numbers.map(x => x % 2 == 0)
  }

  def isDivisibleBy22(numbers: List[Int]): Boolean = {
    numbers.forall(x => x % 2 == 0)
  }

  def countChar(str: String): Int =
  str.groupBy(identity).mapValues(_.size)('a')


  val cost = (1 to 8).reduce((acc, _) => acc + 8)
  val costs = (1 to 8).map(_ => 8).sum


  // Two data lists given; select common items
  private def commonList(list1: List[Int], list2: List[Int]): List[Int] ={
    list1.foldLeft(List[Int]())((acc,i) => if(list2.contains(i)) i :: acc else acc).reverse
  }
  private val l11 = List(1,2,3,4,5)
  private val l22 = List(3,4)
  println(commonList(l11,l22))


  // 1. Create Coupon Codes - use own logic
  import scala.util.Random
  private val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  private val numbers = "0123456789"
  private def generateCouponCode(length: Int): String = {
    val random = new Random()
    (1 to length).map { _ =>
      if (random.nextBoolean()) alphabet(random.nextInt(alphabet.length))
      else numbers(random.nextInt(numbers.length))
    }.mkString("")
  }
  println(generateCouponCode(5))
  println(generateCouponCode(6))

  // 2. Create Coupon Codes - use own logic
  private val timestamp = System.currentTimeMillis()
  private val randomNum = scala.util.Random.nextInt(1000)
  private val userId = 123
  private val coupon = s"$timestamp$randomNum$userId"
  println(coupon)


  def isPalindrome(s: String): Boolean = {
    //s.indices.filter(i => s(i) != s(s.length - i - 1)).forall(_ => false)
    // s.indices.foldLeft(true)((acc, i) => acc && s(i) == s(s.length - i - 1))
    s.indices.forall(i => s(i) == s(s.length - i - 1))
    //s.zipWithIndex.filter{ case (c, i) => c != s(s.length - i - 1) }.forall(_ => false)
  }

  def palindrome(num: Int): Boolean ={
    val s = num.toString
    s.indices.forall(i => s(i) == s(s.length - i - 1))
    //s.zipWithIndex.filter{ case (c, i) => c != s(s.length - i - 1) }.forall(_ => false)
  }

  def isPalindrome(n: Int): Boolean = {
    def reverse(n: Int, acc: Int = 0): Int =
      if (n == 0) acc else reverse(n / 10, acc * 10 + n % 10)
    n == reverse(n)
  }

  def removeDuplicates(arr: Array[Int]): Array[Int] = {
    arr.foldLeft(List[Int]()) { (result, current) => if (result.isEmpty || result.last != current) result :+ current else result}.toArray
  }

  // Number series: [1,2,3,4]; group numbers to get out put as [(1,2),(2,3),(3,5)]
  private def numSeries(list: List[Int]):List[(Int,Int)] ={
    // list.zip(list.tail)
    list.sliding(2).map(x => (x.head, x(1))).toList
  }
  private val l10 = List(1,2,3,4)
  println(numSeries(l10))

  private def problemString(str:String): String ={
    str.split(" ").foldLeft(List[String]())((acc,i) =>
      if(i.length < 4) i.toUpperCase :: acc
      else i.take(i.length-4) + i.takeRight(4).toUpperCase :: acc).reverse.toString
  }
  val k = "My name is Anand kushwaha"
  println(problemString(k))

  private def diffMaxAndMin(arr: Array[Int]): Unit ={
    val a = arr.reduce((x,y) => if(x>y) x else y)
    val b = arr.reduce((x,y) => if(x<y) x else y)
    val maxDiff = arr.reduce((maxDiff,current) => maxDiff max (current - arr.min))
    println(a-b)
    println(maxDiff)
  }
  private val kk = Array(1,2,3,4,5)
  diffMaxAndMin(kk)

  private def stringProblem(str: String, c:Char): Boolean ={
    str.count(_ == c) >= 2 && str.count(_ == c) <= 4
    // val count = str.foldLeft(0)((count, c) => if(c == char) count + 1 else count)
  }






  sortCharacter("Anand")
  private val map1 = Map("hello" -> 1, "hi" -> 2)
  private val map2 = Map("hi" -> 2, "you" -> 1)
  println(merge(map1,map2))

  val l = List(
    Map("a" -> Map("x" -> 1, "y" -> 2)),
    Map("b" -> Map("x" -> 3, "y" -> 4)),
    Map("a" -> Map("x" -> 5, "y" -> 6))
  )

  val r = reduce3(l)
  println(r)
  // Expected output: Map("a" -> Map("x" -> 6, "y" -> 8), "b" -> Map("x" -> 3, "y" -> 4))
  val lis = List(1,2,3,4,5)
  private val obj = new MyList(lis)
  val f = (x:Int) => 5*x
  private val func = (x: Int) => List(x,x+1)
  println(obj.map(f))
  println(obj.flatMap(func))


  val raw = "hello who are you kumar hello you hallo hal"
  println(counter(raw))
  println(counters(raw))
  val list = List(1,2,3,4,4,5,1)
  println(reverse(list,List()))
  println(reverseHOF(list))
  println(show(3,4))
  val arr = Array(1,2,3,4,5)
  maxAndMin(arr)
  println(rotate(arr,3).mkString(" "))
  println(fill(3)(4))
  println(fill1(3)(4))
  println(removeDuplicates(list))
  println(removeDuplicate(list))
  val s = "hello how are you hello"
  println(count(s))
  import dataStructure.Arrays.ListImp.RichInt
  val num = 2
  val square = num.square // 4
  println(square)
  val count = countFiles("/path/to/directory")
  println(count)

  val map = Map("anand" -> 2, "sonu" -> 1, "akanksha" -> 5)
  println(removeOdd(map))

  val list2 = List(5,6,7)
  println(concatList(list,list2))


  println(concatenate(map1,map2))


  val l1 = List(1,2,3)
  val l2 = List("One", "Two")
  println(zip(l1,l2))
}
