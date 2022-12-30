package mixedProblem

import scala.annotation.tailrec

object fibonacci {
  private def fib(num: Int): Unit = {
    var t1 = 0
    var t2 = 1
    var nextTerm = t1 + t2
    print("Fibonacci Series:  ")
    printf("%d ",t1)
    printf("%d ",t2)
    for(i <- 3 to num){
      printf("%d ", nextTerm);
      t1 = t2
      t2 = nextTerm
      nextTerm = t1 + t2
    }
  }

  private def fibTillNth(num: Int): Unit = {
    var t1 = 0
    var t2 = 1
    var nextTerm = t1 + t2
    print("Fibonacci Series:  ")
    printf("%d ", t1)
    printf("%d ", t2)
    while(nextTerm <= num) {
      printf("%d ", nextTerm)
      t1 = t2
      t2 = nextTerm
      nextTerm = t1 + t2
    }
  }

  private def fibUsingRec(num: Int): Int = {
    if(num == 0) return 0
    if(num == 1) return 1
    else fibUsingRec(num-1) + fibUsingRec(num-2)

  }

  private def fibNthUsingRec(num: Int): Int = {
    if(num == 1) return 0
    if(num == 2) return 1
    else fibNthUsingRec(num-1) + fibNthUsingRec(num-2)
  }

  @tailrec
  private def fibonacciUsingTailRec(n: Int, a: Int, b: Int): Unit = {
    if (n > 0) {
      println(a)
      fibonacciUsingTailRec(n - 1, b, a + b)
    }
  }


  def main(args: Array[String]): Unit = {
    val term = scala.io.StdIn.readInt()
    //fib(term)
    //fibTillNth(term)

//    print("Fibonacci Series: ")
//    for(i <- 0 until term){
//      printf("%d ",fibUsingRec(i))
//    }
//
//    println(fibNthUsingRec(10))

      println(fibonacciUsingTailRec(term,0,1))

  }

}
