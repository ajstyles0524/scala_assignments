package dataStructure.stack

object ImportantStackProblems {

  private def isBalanced(string: String): Boolean = {
    val stack = scala.collection.mutable.Stack[Char]()
    for (char <- string) {
      if (char == '(' || char == '[' || char == '{') stack.push(char)
      else if (char == ')' || char == ']' || char == '}') {
        if (stack.isEmpty) {
          return false
        }
        val top = stack.pop()
        if ((top == '(' && char != ')') || (top == '[' && char != ']') || (top == '{' && char != '}')) {
          return false
        }
      }
    }
    stack.isEmpty
  }

  def reverse(string: String): String = {
    val stack = scala.collection.mutable.Stack[Char]()
    for (char <- string) {
      stack.push(char)
    }
    var reversed = ""
    while (stack.nonEmpty) {
      reversed += stack.pop()
    }
    reversed
  }

  def eval(expression: String): Double = {
    val stack = scala.collection.mutable.Stack[Double]()
    for (token <- expression.split(" ")) {
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
    }
    stack.pop()
  }

  def infixToPostfix(infix: String): String = {
    val stack = new scala.collection.mutable.Stack[Char]
    val postfix = new StringBuilder

    for (c <- infix) {
      c match {
        case '+' | '-' | '*' | '/' | '^' =>
          while (stack.nonEmpty && precedence(c) <= precedence(stack.top)) {
            postfix.append(stack.pop)
          }
          stack.push(c)
        case '(' => stack.push(c)
        case ')' =>
          while (stack.nonEmpty && stack.top != '(') {
            postfix.append(stack.pop)
          }
          if (stack.nonEmpty) stack.pop
        case _ => postfix.append(c)
      }
    }

    while (stack.nonEmpty) {
      postfix.append(stack.pop)
    }

    postfix.toString
  }

  def precedence(c: Char): Int = c match {
    case '+' | '-' => 1
    case '*' | '/' => 2
    case '^' => 3
    case _ => 0
  }


  def main(args:Array[String]):Unit ={
    println(isBalanced("()"))
    println(reverse("Anand Jaiswal"))
    println(eval("2 3 1 * + 9 -"))
    println(infixToPostfix("2 + 3 * 4"))
  }
}
