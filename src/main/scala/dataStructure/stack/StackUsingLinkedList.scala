package dataStructure.stack
import scala.annotation.tailrec

class StackUsingLinkedList[T]{
  private class Node(val value: T, var next: Node = null)
  private var top: Node = null

  def push(value: T): Unit = {
    if(top == null) top = new Node(value)
    else{
      val temp = top
      top = new Node(value)
      top.next = temp
    }
    println(value + " is pushed into stack")
  }

  def pop(): T = {
    val result = top.value
    println(top.value + " is popped from stack")
    top = top.next
    result
  }

  def isEmpty: Boolean = top == null

  def peek(): Unit ={
    if(top == null) println("Stack is empty")
    else println(top.value + " is peak element")
  }

  def printStack(): Unit = {
    @tailrec
    def printStackRec(top: Node): Unit = {
      if (top == null) println()
      else {
        print(top.value + " ")
        printStackRec(top.next)
      }
    }
    printStackRec(top)
  }
}

object stack extends App{
  val stack = new StackUsingLinkedList[Int]
  println(stack.isEmpty)
  stack.push(1)
  stack.push(2)
  stack.push(3)
  stack.push(4)
  stack.push(6)
  stack.printStack()
  stack.peek()
  println(stack.pop())
  stack.push(5)
  stack.push(6)
  stack.push(7)
  stack.push(8)
  stack.push(9)
  stack.push(10)
  stack.push(5)
  stack.printStack()
}