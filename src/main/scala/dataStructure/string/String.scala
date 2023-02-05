package dataStructure.string

object String extends App{

  // https://www.w3resource.com/scala-exercises/string/scala-string-exercise-24.php
  def length(str: String): Int ={
    var l = 0
    for (c <- str) l+= 1
    l
  }

  private def findLength(s: String): Int = {
    s.foldLeft(0)((acc, _) => acc + 1)
  }



  def reverse(str: String): String = {
    var reversed = ""
    for (i <- 0 until str.length) {
      val value = str.charAt(i)
      reversed = value + reversed
    }
    reversed
  }

  def reverseString(s: String): String = {
    s.foldLeft("")((acc, c) => c + acc)
  }

  def reverseRec(str: String, index: Int, n: Int): Unit = {
    if (index == n) return
    else {
      val temp = str(index)
      reverseRec(str, index + 1, n)
      print(temp)
    }
  }




  private def WordsInReverse(str1: String): String = {
    val each_words = str1.split(" ")
    var revString = ""
    for (i <- 0 until each_words.length) {
      val word = each_words(i)
      var reverseWord = ""
      for (j <- word.length - 1 to 0 by -1) {
        reverseWord = reverseWord + word.charAt(j)
      }
      revString = revString + reverseWord + " "
    }
    revString
  }

  private def WordsInReverseHOF(str: String): String ={
    str.split(" ").map(x => x.foldLeft("")((acc,i)=> i+acc)).mkString(" ")
  }

  def reverseWordsRec(s: String): String = {
    s.split(" ").foldLeft("")((acc, word) => word + " " + acc).trim
  }


  def upper(str: String): String ={
    var upper = ""
    for (c <- str) {
      if (c >= 'a' && c <= 'z') {
        upper += (c - 32).toChar
      } else {
        upper += c
      }
    }
    upper
  }

  def toUpperCase(s: String): String = {
    s.map(_.toUpper)
  }

  def isPalindrome(str: String):Boolean ={
    var isPalindrome = true
    var i = 0
    while (i < str.length / 2) {
      if (str(i) != str(str.length - i - 1)) {
        isPalindrome = false
        return false
      }
      i += 1
    }
    if (isPalindrome) true else false
  }

  def checkPalindrome(s: String): Boolean = {
    s.zip(s.reverse).forall(c => c._1 == c._2)
  }


  private def countWords(str: String): Int ={
    var count = 0
    var inWord = false
    val trimmedSentence = str.trim
    for (c <- trimmedSentence) {
      if (c != ' ' && !inWord) {
        count += 1
        inWord = true
      } else if (c == ' ') {
        inWord = false
      }
    }
   count
  }


  def countWordsInSentence(s: String): Int = {
    s.split(" ").foldLeft(0)((acc, _) => acc + 1)
  }

  private def removeAdjacentDuplicates(s: String): String = {
    var output = ""
    var lastChar = ' '
    for (c <- s) {
      if (c != lastChar) {
        output += c
        lastChar = c
      }
    }
    output
  }

  private def removeAdjacent(s: String): String = {
    s.foldRight("")((c,acc) => if (acc.headOption.contains(c)) acc else c + acc)
  }



  private def countVowels(s: String): Int = {
    val vowels = Set('a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U')
    s.count(c => vowels.contains(c))
    s.foldLeft(0)((acc,i) => if(vowels.contains(i)) acc+1 else acc )
  }

  private def findSubstring(s: String, sub: String): Int = {
    val sLength = s.length
    val subLength = sub.length
    for (i <- 0 to sLength - subLength) {
      var j = 0
      while (j < subLength && s(i + j) == sub(j)) j += 1
      if (j == subLength) return i
    }
    -1
  }

  def findSubstring3(s: String, search: String): Boolean = {
    s.sliding(search.length).contains(search)
  }

  private def removeDuplicate(s: String): String = {
    s.foldLeft("")((acc,i) => if(acc.contains(i)) acc else acc + i)
  }

  private def showDuplicates(str1: String): Unit = {
    val MAX_CHARS = 256;
    val ctr = new Array[Int](MAX_CHARS);
    for (i <- 0 until str1.length)
      ctr(str1.charAt(i)) = ctr(str1.charAt(i)) + 1;
    for (i <- 0 until MAX_CHARS)
      if (ctr(i) > 1)
        printf("%c appears %d times\n", i, ctr(i));
  }

  def printDuplicates(str:String): Unit ={
    str.groupBy(identity).filter(_._2.length > 1).keys.foreach(println)
  }

  private def countDuplicates(str: String): Unit = {
    val duplicates = str.groupBy(identity).filter(_._2.length > 1)
    duplicates.foreach(x => println(s"${x._1} : ${x._2.length}"))
  }

  def maxOccurring(str1: String): Char = {
    val N = 256;
    val ctr = new Array[Int](N)
    val l = str1.length()
    for (i <- 0 until l)
      ctr(str1.charAt(i)) = ctr(str1.charAt(i)) + 1
    var max = Int.MinValue
    var result = ' '
    for (i <- 0 until l) {
      if (max < ctr(str1.charAt(i))) {
        max = ctr(str1.charAt(i))
        result = str1.charAt(i)
      }
    }
    result
  }
  // codePointAt == unicode
  // equals or equalsIgnoreCase
  // my name is Anand Kumar
  private def solution(str: String): String ={
    str.split(" ").foldLeft(List[String]())((acc,i) =>
      if(i.length <= 4) acc :+ i
      else {
        val s = i.take(i.length-4) + i.takeRight(4).toUpperCase
        acc :+ s
      }
    ).toString
  }

  val s = "My name is Debdeep Goswami"
  println(solution(s))



  def maxOccurringChar(str: String): Unit = {
    val charCount = str.groupBy(identity).filter(_._2.length > 1).map(x => (x._1, x._2.length))
    val maxChar = charCount.maxBy(_._2)
    println(s"The maximum occurring character is '${maxChar._1}' with a count of ${maxChar._2}.")
  }

  def checkForRotation(str1: String, str2: String): Boolean = {
    (str1.length == str2.length) && ((str1 + str1).indexOf(str2) != -1);
  }

  def isAnagram(s: String, t: String): Boolean = {
    if (s.length != t.length) return false
    val sChars = s.toCharArray
    val tChars = t.toCharArray
    val charCount = Array.fill(26)(0)
    sChars.foreach(c => charCount(c - 'a') += 1)
    tChars.foreach(c => charCount(c - 'a') -= 1)
    charCount.forall(_ == 0)
  }

  def isAnagramHOF(s: String, t: String): Boolean = {
    if (s.length != t.length) return false
    s.groupBy(identity) == t.groupBy(identity)
  }

  private def sortByFrequency(s: String): String = {
    val charCount = s.groupBy(identity).mapValues(_.length)
    s.sortWith((a, b) =>
      if (charCount(a) == charCount(b)) a < b
      else charCount(a) > charCount(b)
    )
  }

  private def vowelAndCons(str: String): Unit ={
    val vowels = str.count(c => "aeiouAEIOU".contains(c))
    val consonants = str.count(c => !"aeiouAEIOU".contains(c))
    println("Number of vowels: " + vowels)
    println("Number of consonants: " + consonants)
  }

  private def countChar(str: String, char: Char): Unit ={
    val count = str.count(_ == char)
    println("Number of '" + char + "' in '" + str + "': " + count)
  }

  private def firstNonRepeating(str: String): Unit ={
    val firstNonRepeated = str.groupBy(identity).find(elem => elem._2.size == 1).map(_._1)
    println("First non-repeated character: " + firstNonRepeated)
  }

  def removeChar(str: String): Unit ={
    val charToRemove = 'l'
    var newStr = ""
    for (c <- str) {
      if (c != charToRemove) {
        newStr += c
      }
    }
    println(newStr)
  }

  def removeCharUsingHOF(str: String): Unit={
    val charToRemove = 'l'
    val newStr = str.filter(c => c != charToRemove)
    println(newStr)
  }

  private def maxOccurringWord(arr: Array[String]): Unit ={
    val grouped = arr.groupBy(identity).map(x => (x._1,x._2.length))
    val mostFrequent = grouped.maxBy(_._2)
    println(mostFrequent._1)
  }



  //println(reverseWords("Anand Jaywalk"))
  println(WordsInReverse("Anand swap"))
  println()

  private val inputString = "aabbccddeeff"
  private val outputString = removeAdjacentDuplicates(inputString)
  println(s"Original string: $inputString")
  println(s"String with duplicates removed: $outputString")
  println()

  val input = "Hello, world!"
  private val vowelCount = countVowels(input)
  println(s"The string '$input' contains $vowelCount vowels.")
  println()

  val string = "Hello, world!"
  private val substring = "world"
  val index = findSubstring(string, substring)
  if (index != -1) println(s"The substring '$substring' was found at index $index in the string '$inputString'.")
  else println(s"The substring '$substring' was not found in the string '$inputString'.")
  println()

  showDuplicates(string)

  vowelAndCons(input)

  countChar("Anand",'n')

  firstNonRepeating("AAAABBCDD")

  //println(wordReverse("I am play boy"))

  val arr = Array("Hello", "world", "Hello", "scala", "Hello", "world")
  maxOccurringWord(arr)

  val str = "abcabcbb"
  //longestSubString(str)

}
