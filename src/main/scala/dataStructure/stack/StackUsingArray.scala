package dataStructure.stack

import scala.annotation.tailrec
import scala.collection.mutable

// It is a linear data structure that follows a particular order in which the operations are performed.
// To implement the stack, it is required to maintain the pointer to the top of the stack,
// which is the last element to be inserted because we can access the elements only on
// the top of the stack.
// Redo-undo features at many places like editors, photoshop.
// Forward and backward features in web browsers

class StackUsingArray {
  private val max: Int = 10
  private var top: Int = -1
  val arr: Array[Int] = new Array[Int](max)

  private def push(ele: Int): Boolean ={
    if(top >= max-1){
      println("Stack Overflow Error!")
      false
    }
    else{
      top += 1
      arr(top) = ele
      println(ele + " Pushed into stack")
      true
    }
  }

  private def pop(): Int ={
    if(top < 0) {
      printf("Stack Underflow Error! ")
      0
    }
    else{
      val x = arr(top)
      top -= 1
      x
    }
  }

  def empty(): Boolean ={
    if(top < 0) true
    else false
  }

  private def peek(): Int ={
    if(top < 0) {
      println("Stack Underflow Error!")
      0
    }
    else{
      arr(top)
    }
  }

  private def printStack(): Unit = {
    @tailrec
    def printStackRec(stack:Array[Int], top:Int):Unit ={
      if(top < 0) println()
      else{
        print(stack(top) + " ")
        printStackRec(stack, top-1)
      }
    }
    printStackRec(arr,top)
  }
}

object StackUsingArray extends App{
  val stack = new StackUsingArray
  println(stack.empty())
  stack.push(1)
  stack.push(2)
  stack.push(3)
  stack.push(4)
  stack.push(6)
  stack.printStack()
  println(stack.peek())
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

//Advantages of array implementation:
//    Easy to implement.
//    Memory is saved as pointers are not involved.

//Disadvantages of array implementation:
//
//    It is not dynamic i.e., it doesn’t grow and shrink depending on needs at runtime.
//    The total size of the stack must be defined beforehand.