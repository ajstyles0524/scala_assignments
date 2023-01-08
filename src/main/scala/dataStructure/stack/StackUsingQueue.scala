package dataStructure.stack
import scala.annotation.tailrec
import scala.collection.mutable.Queue
class StackUsingQueue[T] {

  private val queue = Queue.empty[T]


  def push(x: T): Unit = {
    queue.enqueue(x)
    rotate(queue.size - 1)
  }

  def rotate(n: Int): Unit = {
    if (n > 0) {
      queue.enqueue(queue.dequeue())
      rotate(n - 1)
    }
  }

  def pop(): Option[T] = {
    if (queue.isEmpty) {
      None
    } else {
      Some(queue.dequeue())
    }
  }

  def peek: Option[T] = {
    queue.headOption
  }


  def isEmpty: Boolean = {
    queue.isEmpty
  }

  def printElements(): Unit = {
    queue.foreach(println)
  }

}


object StackUsingQueue extends App{
  val stack = new StackUsingQueue[Int]
  stack.push(1)
  stack.push(2)
  stack.push(3)
  println(stack.peek) // prints 3
  stack.printElements()
  println()
  println(stack.pop()) // prints 3
  println(stack.pop()) // prints 2
  println(stack.pop()) // prints 1
}


// 3 2 1
// 1 2 3