package dataStructure.stack

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.util.control.Breaks.{break, breakable}
object ImportantStackProblems {

  // string = ([{}])
  private def isBalanced(string: String): Boolean = {
    val stack = Stack[Char]()
    @tailrec
    def check(remaining: List[Char], stack: Stack[Char]): Boolean = {
      if (remaining.isEmpty) stack.isEmpty
      else {
        val char = remaining.head
        if (char == '(' || char == '[' || char == '{') check(remaining.tail, stack.push(char))
        else if (char == ')' || char == ']' || char == '}') {
          if (stack.isEmpty) return false
          val top = stack.pop()
          if ((top == '(' && char != ')') || (top == '[' && char != ']') || (top == '{' && char != '}'))  false
          else check(remaining.tail, stack)
        }
        else {
          check(remaining.tail, stack)
        }
      }
    }
    check(string.toList, stack)
  }

  def reverse(string: String): String = {
    val stack = scala.collection.mutable.Stack[Char]()
    @tailrec
    def reverse(str: String): String = {
      if (str.isEmpty) {
        stack.pop().toString
      } else {
        stack.push(str.head)
        reverse(str.tail)
      }
    }
    @tailrec
    def buildReversedString(acc: String): String = {
      if (stack.isEmpty) acc
      else buildReversedString(acc + stack.pop())
    }
    buildReversedString(reverse(string))
  }

  private def reverseWordWise(string: String): String ={
    string.split(" ").map(reverse).mkString(" ")
  }

  private def deleteMiddle(stack: mutable.Stack[Int], curr: Int, n: Int): Unit = {
    if (stack.isEmpty || curr == n) return
    val x = stack.pop()
    deleteMiddle(stack, curr + 1, n)
    if (curr != n/2) stack.push(x)
  }


  private def evalPostfixExp(expression: String): Double = {
    val stack = scala.collection.mutable.Stack[Double]()
      @tailrec
      def eval(expression: List[String]):Double = {
        if(expression.isEmpty) stack.pop()
        else{
          val token = expression.head
          token match {
          case "+" =>
            val right = stack.pop()
            val left = stack.pop()
            stack.push(left + right)
          case "-" =>
            val right = stack.pop()
            val left = stack.pop()
            stack.push(left - right)
          case "*" =>
            val right = stack.pop()
            val left = stack.pop()
            stack.push(left * right)
          case "/" =>
            val right = stack.pop()
            val left = stack.pop()
            stack.push(left / right)
          case _ =>
            stack.push(token.toDouble)
        }
          eval(expression.tail)
       }
      }
    eval(expression.split(" ").toList)
  }

  private def reverseStack(stack: Stack[Int]): Stack[Int] = {
    if (stack.isEmpty) {
      stack
    } else {
      val x = stack.pop()
      reverseStack(stack)
      stack.push(x)
    }
  }


  // infix to postfix

  // 1. Initialize an empty stack.
  // 2. Iterate through each character in the infix expression:
  //      2(a).If the character is an operand, append it to the postfix expression.
  //      2(b).If the character is an operator, pop any operators from the stack that have
  //           higher or equal precedence and append them to the postfix expression. Then push the character onto the stack.
  //      2(c).If the character is a left parenthesis, push it onto the stack.
  //      2(d).If the character is a right parenthesis, pop all operators from the stack until you reach a left parenthesis.
  //           Discard both the left and right parenthesis.
  // 3.After the infix expression has been completely processed, check the stack.
  // If there are any operators remaining on the stack, pop them and append them to the postfix expression.
  private def infixToPostfix(expression: String): String = {
    val stack = new Stack[Char]
    val result = new StringBuilder
    def processOperator(op: Char): Unit = {
      while (stack.nonEmpty && precedence(op) <= precedence(stack.top)) {
        result.append(stack.pop)
      }
      stack.push(op)
    }
    def precedence(op: Char): Int = op match {
      case '+' | '-' => 1
      case '*' | '/' => 2
      case '^' => 3
      case _ => -1
    }
    def processChar(ch: Char): Unit = {
      ch match {
        case ' ' =>
        case '(' => stack.push(ch)
        case ')' =>
          while (stack.top != '(') {
            result.append(stack.pop)
          }
          stack.pop()
        case op if "+-*/^".indexOf(op) >= 0 => processOperator(op)
        case _ => result.append(ch)
      }
    }

    @tailrec
    def processExpression(expr: String): Unit = {
      if (expr.nonEmpty) {
        processChar(expr.head)
        processExpression(expr.tail)
      }
    }
    processExpression(expression)

    while (stack.nonEmpty) {
      result.append(stack.pop)
    }

    result.toString
  }



  // infix to prefix
  // Step 1: Reverse the infix expression i.e A+B*C will become C*B+A. Note while reversing each ‘(‘ will become ‘)’ and each ‘)’ becomes ‘(‘.
  // Step 2: Obtain the “nearly” postfix expression of the modified expression i.e CB*A+.
  // Step 3: Reverse the postfix expression. Hence in our example prefix is +A*BC.
  private def infixToPrefix(expression: String): String = {
    val stack = new Stack[Char]
    var result = ""
    @tailrec
    def convert(expression: String): Unit = {
      if (expression.nonEmpty) {
        val ch = expression.last
        if (ch.isLetterOrDigit) {
          result = ch + result
        } else if (ch == ')') {
          stack.push(ch)
        } else if (ch == '(') {
          while (stack.nonEmpty && stack.top != ')') {
            result = stack.pop + result
          }
          stack.pop
        } else {
          while (stack.nonEmpty && precedence(ch) < precedence(stack.top)) {
            result = stack.pop + result
          }
          stack.push(ch)
        }
        convert(expression.init)
      }
    }

    convert(expression)
    while (stack.nonEmpty) {
      result = stack.pop + result
    }
    result
  }

  private def precedence(ch: Char): Int = {
    ch match {
      case '+' | '-' => 1
      case '*' | '/' => 2
      case '^' => 3
      case '(' | ')' => 4
    }
  }




  // prefix to postfix
  // Read the Prefix expression in reverse order (from right to left)
  // If the symbol is an operand, then push it onto the Stack
  // If the symbol is an operator, then pop two operands from the Stack
  // Create a string by concatenating the two operands and the operator after them.
  // string = operand1 + operand2 + operator
  // And push the resultant string back to Stack
  // Repeat the above steps until end of Prefix expression

  private def prefixToPostfix(expression: String): String = {
    val stack = new Stack[String]
    @tailrec
    def convert(expression: String): String = {
      if (expression.nonEmpty) {
        val ch = expression.last
        if (ch.isLetterOrDigit) {
          stack.push(ch.toString)
        } else {
          if (stack.size >= 2) {
            val op1 = stack.pop
            val op2 = stack.pop
            stack.push(op1 + op2 + ch)
          } else {
            throw new IllegalArgumentException("Invalid prefix expression")
          }
        }
        convert(expression.init)
      } else {
        if (stack.size == 1) {
          stack.pop
        } else {
          throw new IllegalArgumentException("Invalid prefix expression")
        }
      }
    }

    convert(expression)
  }



  // prefix to infix
  // Read the Prefix expression in reverse order (from right to left)
  // If the symbol is an operand, then push it onto the Stack
  // If the symbol is an operator, then pop two operands from the Stack
  // Create a string by concatenating the two operands and the operator between them.
  // string = (operand1 + operator + operand2)
  // And push the resultant string back to Stack
  // Repeat the above steps until the end of Prefix expression.
  // At the end stack will have only 1 string i.e resultant string
  private def prefixToInfix(expression: String): String = {
    val stack = new Stack[String]
    @tailrec
    def convert(expression: String): String = {
      if (expression.nonEmpty) {
        val ch = expression.last
        if (ch.isLetterOrDigit) {
          stack.push(ch.toString)
        } else {
          val op1 = stack.pop
          val op2 = stack.pop
          stack.push("(" + op1 + ch + op2 + ")")
        }
        convert(expression.init)
      } else {
        stack.pop
      }
    }
    convert(expression)
  }


  // postfix to infix

  // 1.While there are input symbol left
  //    …1.1 Read the next symbol from the input.
  // 2.If the symbol is an operand
  //    …2.1 Push it onto the stack.
  // 3.Otherwise,
  //    …3.1 the symbol is an operator.
  //    …3.2 Pop the top 2 values from the stack.
  //    …3.3 Put the operator, with the values as arguments and form a string.
  //    …3.4 Push the resulted string back to stack.
  // 4.If there is only one value in the stack
  //    …4.1 That value in the stack is the desired infix string.
  private def postfixToInfix(expression: String): String = {
    val stack = new Stack[String]

    @tailrec
    def convert(expression: String): String = {
      if (expression.nonEmpty) {
        val ch = expression.head
        if (ch.isLetterOrDigit) {
          stack.push(ch.toString)
        } else {
          val op2 = stack.pop
          val op1 = stack.pop
          stack.push("(" + op1 + ch + op2 + ")")
        }
        convert(expression.tail)
      } else {
        stack.pop
      }
    }
    convert(expression)
  }

  // postfix to prefix

  // Read the Postfix expression from left to right
  // If the symbol is an operand, then push it onto the Stack
  // If the symbol is an operator, then pop two operands from the Stack
  // Create a string by concatenating the two operands and the operator before them.
  // string = operator + operand2 + operand1
  // And push the resultant string back to Stack
  // Repeat the above steps until end of Postfix expression.
  private def postfixToPrefix(expression: String): String = {
    val stack = new Stack[String]
    @tailrec
    def convert(expression: String): String = {
      if (expression.nonEmpty) {
        val ch = expression.head
        if (ch.isLetterOrDigit) {
          stack.push(ch.toString)
        } else {
          val op2 = stack.pop
          val op1 = stack.pop
          stack.push(ch + op1 + op2)
        }
        convert(expression.tail)
      } else {
        stack.pop
      }
    }
    convert(expression)
  }



  // Next Greater Elements
  // input = [ 4 , 5 , 2 , 25 ]
  // output = [5, 25, 25, -1]

  private def nextGreater(arr: Array[Int]): Array[Int] = {
    val stack = scala.collection.mutable.Stack[Int]()
    val nge = Array.fill(arr.length)(-1)
    for (i <- arr.indices) {
      while (stack.nonEmpty && arr(i) > arr(stack.top)) {
        nge(stack.pop()) = arr(i)
      }
      stack.push(i)
    }
    nge
  }
  // in functional way
  private def nextGreaterElement(arr: Array[Int]): Array[Int] = {
    val stack = Stack[Int]()
    val nge = Array.fill(arr.length)(-1)
    @tailrec
    def findNGE(i: Int): Unit = {
      if (i < arr.length) {
        if (stack.nonEmpty && arr(stack.top) < arr(i)) {
          nge(stack.pop()) = arr(i)
          findNGE(i)
        } else {
          stack.push(i)
          findNGE(i + 1)
        }
      }
    }
    findNGE(0)
    nge
  }


  private def NGECircular(arr: Array[Int]): Array[Int] ={
    val stack = new Stack[Int]()
    val nge = Array.fill(arr.length)(-1)
    val n = arr.length
    @tailrec
    def findNGE(i: Int): Unit ={
      if(i < 2*n){
        if(stack.nonEmpty && arr(i % n) > arr(stack.top)){
          nge(stack.top) = arr(i % n)
          stack.pop()
          findNGE(i)
        }
        else{
          stack.push(i%n)
          findNGE(i+1)
        }
      }
    }
    findNGE(0)
    nge
  }


  private def nextSmallestElement(arr: Array[Int]): Array[Int] = {
    val stack = Stack[Int]()
    val nse = Array.fill(arr.length)(-1)

    @tailrec
    def findNSE(i: Int): Unit = {
      if (i < arr.length) {
        if (stack.nonEmpty && arr(stack.top) > arr(i)) {
          nse(stack.pop()) = arr(i)
          findNSE(i)
        } else {
          stack.push(i)
          findNSE(i + 1)
        }
      }
    }
    findNSE(0)
    nse
  }


  // 4 5 2 25
  private def countOFNGE(arr: Array[Int],index: Int): Int = {
    var count = 0
    @tailrec
    def findNGE(i: Int,ele:Int): Unit = {
      if (i < arr.length) {
        if (ele < arr(i)) {
          count += 1
          findNGE(i+1,ele)
        }
        else findNGE(i + 1, ele)
      }
    }
    findNGE(index+1,arr(index))
    count
  }


  // arr = {3,0,0,2,0,4}
  // breaking into smaller parts
  //
  // for i = 0, left = 3, right = 4, ans = 0
  // for i = 1, left = 3, right = 4, ans = 3
  // for i = 2, left = 3, right = 4, ans = 6
  // for i = 3, left = 3, right = 4, ans = 7
  // for i = 4, left = 3, right = 4, ans = 10
  // for i = 5, left = 4, right = 4, ans = 10

  //    (brute force)
  //    var ans = 0
  //    val length = arr.length
  //    for (i <- 0 until length) {
  //      var left = 0
  //      var right = 0
  //      for (j <- i to 0 by -1) {
  //        left = Math.max(arr(j), left)
  //      }
  //      for (k <- i until length) {
  //        right = Math.max(right, arr(k))
  //      }
  //      ans = ans + Math.abs(Math.min(left, right) - arr(i))
  //    }
  //    ans




  def trappingWater(height: Array[Int]): Int ={
    if (height.length == 0) return 0

    var left: Int = 0
    var right: Int = height.length - 1
    var maxLeft: Int = 0
    var maxRight: Int = 0
    var totalWater: Int = 0
    while (left < right) {
      if (height(left) < height(right)) {
        if (height(left) >= maxLeft) {
          maxLeft = height(left)
        }
        else {
          totalWater += maxLeft - height(left)
        }
        left += 1
      }
      else {
        if (height(right) >= maxRight) {
          maxRight = height(right)
        }
        else {
          totalWater += maxRight - height(right)
        }
        right -= 1
      }
    }
    totalWater
  }


  def main(args:Array[String]):Unit ={
    println(isBalanced("([{)])"))
    println(reverse("Anand Jaiswal"))
    println(reverseWordWise("Anand Jaiswal"))
    println(evalPostfixExp("100 200 + 2 / 5 * 7 +"))

    val newStack = Stack(1,2,3,4,5)
    println(newStack)
    val reversedStack = reverseStack(newStack)
    println(reversedStack)

    val stack = mutable.Stack(1, 2, 3, 4, 5)
    val n = stack.size
    println(deleteMiddle(stack,0,n))
    println(infixToPostfix("a+b*(c^d-e)^(f+g*h)-i"))
    println(infixToPrefix("x+y*z/w+u"))
    println(prefixToPostfix("*-A/BC-/AKL"))
    println(prefixToInfix("*-A/BC-/AKL"))
    println(postfixToInfix("ab*c+"))
    println(postfixToPrefix("ABC/-AK/L-*"))
    val arr = Array(4,5,2,25)
    println(arr.indices.reverse.toList)
    println(nextGreater(arr).mkString(" "))
    println(nextGreaterElement(arr).mkString(" "))
    val array = Array(5,4,3,2,1)
    println(NGECircular(array).mkString(" "))
    val a = Array(5, 6, 2, 3, 1,7)
    println(nextSmallestElement(a).mkString(" "))
    println(countOFNGE(arr,0))
  }
}
