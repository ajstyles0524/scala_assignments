package dataStructure.string

import scala.annotation.tailrec

object W3SchoolQues extends App{

  // Write a Scala program to print after removing duplicates from a given string.
  def removeDuplicates(str: String): String ={
    str.foldLeft("")((acc,i) => if(acc.indexOf(i) == -1) acc + i else acc)
  }

  private def makeEachFirstCharCapital(string: String): String = {
    string.split(' ').map(_.capitalize).mkString(" ")
  }
  println(makeEachFirstCharCapital("anand Anand upi se 7u8"))


  // Write a Scala program to find the maximum occurring character in a string
  private def maxOccurringChar(str: String): Unit ={
    println(str.groupBy(identity).filter(_._2.length > 1).map(x => (x._1,x._2.length)).maxBy(_._2)._1)
  }

  // Write a Scala program to reverse every word in a given string.
  def reverse(str: String): String ={
    str.foldLeft("")((acc,i) => i + acc)
  }

  // Write a Scala program to count and print all the duplicates in the input string.
  private def countAndPrint(str: String): Unit ={
    str.groupBy(identity).filter(_._2.length > 1).map(x => (x._1,x._2.length)).
      foreach(ele => println(s"${ele._1} appears ${ele._2} times"))
  }

  // Write a Scala program to read a string and return true if it ends with a specified string of length 2.
  private def endTwo(str: String, str1: String): Boolean ={
    if(str.length < 2) false else str1.equals(str.substring(str.length-2, str.length))
  }

  // Write a Scala program to read two strings append them together and return the result.
  // If the length of the strings is different remove characters from the beginning of longer string and make them equal length.

  private def makeString(str1:String, str2:String): String ={
    if(str1.length == str2.length) str1 + str2
    else if(str1.length > str2.length) str1.substring(str1.length-str2.length,str1.length) + str2 else str1 + str2.substring(str2.length - str1.length, str2.length)
  }

  // Write a Scala program to check whether the first two characters present at the end of a given string.

  private def checkString(str: String): Boolean ={
    if(str.length < 2) false else str.take(2).equals(str.takeRight(2))
  }

  // Write a Scala program to read a given string and if the first or last characters are same return the string
  // without those characters otherwise return the string unchanged.

  def modifyString(str: String,char: Char): String ={
    if(str.isEmpty) str
    else if(str.length == 1 && str(0) == char) ""
    else if(str(0) == char) str.substring(1,str.length)
    else if(str(str.length) == char) str.substring(0,str.length-1)
    else if(str(0) == str(str.length) == char) str.substring(1,str.length-1)
    else str
  }


  // Write a Scala program to read a string and return the string without the first two characters.
  // Keep the first char if it is 'g' and keep the second char if it is 'h'.
  private def removeFirstTwo(str: String): String = {
    val firstChar = str.head
    val secondChar = str.tail.head

    (firstChar, secondChar) match {
      case ('g', 'h') => str
      case (_, 'h') => str.tail
      case ('g',_) => str(0) + str.substring(2,str.length)
      case _ => str.tail.tail
    }
  }

  private def trimFirstChars(str: String, char: Char): String = {
    @tailrec
    def trimChar(str: String, char: Char): String =
      if (str.nonEmpty && str.head == char) trimChar(str.tail, char) else str
    if (str.length < 2) trimChar(str, char)
    else {
      val trimmed = trimChar(str, char)
      trimChar(trimmed.tail, char)
    }
  }


    // a b c d e
  // Write a Scala program to read a string and returns after remove a specified character and its immediate left and right characters.
  private def removeCharAndAdjacent(s: String, c: Char): String = {
    val i = s.indexOf(c)
    if (i < 0) s
    else s.take(i - 1) + s.drop(i + 2)
  }

  // Write a Scala program to check two given strings whether any one of them appear at the end of the other string (ignore case sensitivity).
  private def checkEnd(s1: String, s2: String): Boolean = {
    //s1.toLowerCase.endsWith(s2.toLowerCase) || s2.toLowerCase.endsWith(s1.toLowerCase)
    if(s1.length > s2.length)   s2.toLowerCase.equals(s1.substring(s1.length-s2.length,s1.length))
    else s1.toLowerCase.equals(s2.substring(s2.length-s1.length,s2.length))
  }


  // Write a Scala program to check whether a substring appears before a period(.) within a given string.
  def beforePeriod(s: String, sub: String): Boolean = {
    val i = s.indexOf('.')
    if (i < 0) false
    else s.slice(0, i).contains(sub)
  }


  // a b c d e

  // Write a Scala program to check whether a given substring presents in the middle of another given string.
  // Here middle means difference between the number of characters to the left and right of the given substring not more than 1.
  def isMiddle(s: String, sub: String): Boolean = {
    val i = s.indexOf(sub)
    if (i < 0) false
    else {
      val left = s.slice(0, i)
      val right = s.slice(i + sub.length, s.length)
      math.abs(left.length - right.length) <= 1
    }
  }

  // Write a Scala program to count how many times the substring 'life' present at anywhere in a given string.
  // Counting can also happen for the substring 'li?e',any character instead of 'f'.
  def countLife(s: String): Int = {
    val pattern = "li.e".r
    val matches = pattern.findAllMatchIn(s)
    matches.size
  }

  def test(stng: String): Int = {
    var l = stng.length();
    var ctr = 0;
    var firsttwo = "li";
    val lastone = "e";
    if (l < 4)
      return 0;
    for (i <- 0 to l - 3) {
      if (firsttwo.compareTo(stng.substring(i, i + 2)) == 0 && lastone
        .compareTo(stng.substring(i + 3, i + 4)) == 0)
        ctr = ctr + 1;
    }
    ctr
  }


  // Write a Scala program to add a string with specific number of times separated by a substring.
  def repeatStringSep(s: String, sep: String,n: Int): String = {
    def Helper(s: String, sep: String, n: Int, acc: String): String ={
      if(n==0) acc
      else Helper(s, sep, n-1, acc + s + sep)
    }
    Helper(s,sep,n," ")
  }


  // Write a Scala program to repeat a specific number of characters for specific number of times from the last part of a given string.
  def repeatLastChars(s: String, n: Int, m: Int): String = {
    val last = s.takeRight(n)
    s + last * m
  }

  // Write a Scala program to create a new string from a given string
  // after removing the 2nd character from the substring of length three starting with 'z' and ending with 'g' presents in the said string.
  def removeSecondChar(s: String): String = {
    val pattern = "z.g"
    val idx = s.indexOfSlice(pattern)
    if (idx < 0) s
    else s.take(idx + 1) + s.drop(idx + 2)
  }

  def test1(stng: String): String = {
    val len = stng.length;
    var newformstring = "";
    for (i <- 0 until len) {
      newformstring += stng.substring(i, i + 1);
      if (i > 0 && i < len - 1) {
        if (stng.charAt(i - 1) == 'z' && stng.charAt(i + 1) == 'g')
          newformstring = newformstring.substring(0, newformstring.length() - 1);
      }
    }
    newformstring;
  }


  // Write a Scala program to check whether the character immediately before and after a specified character is same in a given string.
  def checkAdjacentChars(s: String, c: Char): Boolean = {
    val idx = s.indexOf(c)
    if (idx <= 0 || idx >= s.length - 1) false
    else s(idx - 1) == s(idx + 1)
  }

  def test2(stng: String, schar: Char): Boolean = {
    var l = stng.length;
    var found = true;
    var tmpString: Char = ' '
    for (i <- 0 until l) {
      tmpString = stng(i)
      if (tmpString.compare(schar) == 0) {
        if (stng.charAt(i - 1) == stng.charAt(i + 1)) {
          found = true;
        } else {
          found = false;

        }
      }
    }
    found;
  }


  // Write a Java program to check whether two strings of length 3 and 4 appear in same number of times in a given string.
  private def countSubstring(s: String, sub: String): Int = s.sliding(sub.length).count(x => x == sub)
  def checkStringCount(s: String, str1: String, str2: String): Boolean = {
    val count1 = countSubstring(s, str1)
    val count2 = countSubstring(s, str2)
    count1 == count2
  }

  def test4(stng: String, str1: String, str2: String): Boolean = {
    val l = stng.length;
    var red = 0;
    var blue = 0;
    for (i <- 0 to l - 3) {
      var tmp = stng.substring(i, i + 3);
      if (tmp.compareTo(str2) == 0)
        red = red + 1;
    }
    for (i <- 0 to l - 4) {
      var tmp = stng.substring(i, i + 4);
      if (tmp.compareTo(str1) == 0)
        blue = blue + 1;
    }
    if (red == blue)
      return true;
    else
      return false;
  }


  // Write a Scala program to create a new string repeating every character twice of a given string.
  def doubleChar(s: String): String = s.flatMap(c => List(c, c)).toString()

  def test5(stng: String): String = {
    val l = stng.length;
    var newstring = "";
    for (i <- 0 until l) {
      newstring += stng.substring(i, i + 1) + stng.substring(i, i + 1);
    }
    newstring
  }


  // Write a Scala program to make a new string from two given string in such a way that, each character of two string will come respectively.
  def mergeString(s1: String, s2: String): String = s1.zip(s2).flatMap(t => List(t._1, t._2)).mkString


  // Write a Scala program to make a new string made of p number of characters from the first of a given string
  // and followed by p-1 number characters till the p is greater than zero.

  def newString(str: String, p: Int): String = {
    @tailrec
    def helper(p: Int, acc: String): String = {
      if (p <= 0) acc
      else helper(p - 1, acc + str.substring(0, p))
    }
    helper(p, "")
  }


  // Write a Scala program to count the number of triples (characters appearing three times in a row) in a given string.
  def countTriples(s: String): Int =
    s.sliding(3).count(triple => triple(0) == triple(1) && triple(1) == triple(2))

  def countTriple(s: String): Int = {
    var count = 0
    for (i <- 0 until s.length - 2) {
      if (s(i) == s(i + 1) && s(i + 1) == s(i + 2)) {
        count += 1
      }
    }
    count
  }


  // Write a Scala program to check whether a specified character is happy or not.
  // A character is happy when the same character appears to its left or right in a string.

  private def isHappy(s: String, c: Char): Boolean =
    s.sliding(2).exists(pair => pair.head == c && pair.last == c)


  //  Write a scala program to check the number of appearances of the two substrings appear any where in the string.
  private def countSubstrings(s: String, substring1: String, substring2: String): (Int, Int) = {
    (s.countSubstring(substring1), s.countSubstring(substring2))
  }
  implicit class StringImprovements(val s: String) {
    def countSubstring(substring: String): Int = {
      s.sliding(substring.length).count(x => x == substring)
    }
  }

  private val input1 = "the quick brown fox jumps over the lazy dog"
  val (count1, count2) = countSubstrings(input1, "the", "fox")
  println(s"'the' appears $count1 times")
  println(s"'fox' appears $count2 times")


  // Write a Scala program to calculate the sum of the numbers appear in a given string.
  private def sumOfNumbers(input: String): Int = {
    // str.filter(c => c.isDigit).map(_.asDigit).sum
    // The input.split("\\D+") expression splits the input string into an array of words using one or more non-digit characters as the delimiter.
    input.split("\\D+").flatMap { word =>
        try Some(word.toInt)
        catch {case _: NumberFormatException => None}
      }.sum
  }
  val s1 = "it 15 is25 a 20string"
  println(sumOfNumbers(s1))


  // Write a Scala program to make a new string with each character of just before and after of a non-empty substring
  // whichever it appears in a non-empty given string.

  private def makeNewString(m_stng: String, t_stng: String): String = {
    val m_st_len = m_stng.length()
    val t_st_len = t_stng.length()
    var fin = ""
    for (i <- 0 to m_st_len - t_st_len) {
      val tmp = m_stng.substring(i, i + t_st_len)
      if (i > 0 && tmp.equals(t_stng))
        fin += m_stng.substring(i - 1, i)
      if (i < m_st_len - t_st_len && tmp.equals(t_stng))
        fin += m_stng.substring(i + t_st_len, i + t_st_len + 1)
    }
    fin
  }
  println(makeNewString("weablcoabmeab", "ab"))






  private val string1 = "azzlea"
  private val character1 = 'z'
  println(s"Is $character1 happy in $string1: ${isHappy(string1, character1)}")

  val input = "hello"
  //println(repeatString(input, 3, "best")) // Output: "hello,hello,hello"

  val s = "Ananda Kumar CTO"
  println(removeDuplicates(s))
  maxOccurringChar(s)
  println(reverse(s))
  countAndPrint(s)
  println(endTwo("String","ng"))
  println(makeString("Welcome","home"))
  println(makeString("Scala","Python"))
  println(checkString("educated"))
  println(removeFirstTwo("ghost"))
  println(removeFirstTwo("photo"))
  println(removeFirstTwo("goat"))
  println(  trimFirstChars("babcrtfg", 'a'))
  val str1 = "test#string"
  val str2 = "sdf$#gyhj#"
  println(removeCharAndAdjacent(str1, '#'))
  println(removeCharAndAdjacent(str2, '#'))
  println(checkEnd("pqrxyz","Xyz"))





}
