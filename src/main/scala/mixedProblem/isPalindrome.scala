package mixedProblem

object isPalindrome {

  private def checkPalindrome(num: Int): Boolean = {
    var temp = num
    var new_num = 0
    while(temp != 0){
      new_num = new_num*10 + (temp%10)
      temp = temp/10
    }
    if(new_num == num) return true
    else false
  }

  def main(args: Array[String]): Unit = {
    val num = scala.io.StdIn.readInt()
    println(checkPalindrome(num))
  }

}
