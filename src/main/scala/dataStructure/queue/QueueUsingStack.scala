package dataStructure.queue

class QueueUsingStack[T] {
  private val stack = new scala.collection.mutable.Stack[T]

  def enqueue(value: T): Unit = {
    stack.push(value)
  }

  def dequeue(): Option[T] = {
    if (stack.isEmpty) {
      None
    } else if (stack.size == 1) {
      Some(stack.pop())
    } else {
      val value = stack.pop()
      val result = dequeue()
      stack.push(value)
      result
    }
  }

  def front(): Option[T] = {
    stack.lastOption
  }

  def rear(): Option[T] = {
    stack.headOption
  }

  def printElements(): Unit = {
    println(stack.mkString(" "))
  }

}

object QueueUsingStack extends App{
  val queue = new QueueUsingStack[Int]
  queue.enqueue(1)
  queue.enqueue(2)
  queue.enqueue(3)
  queue.printElements()
  println(queue.front())
  println(queue.rear())
  println(queue.dequeue()) // prints 1
  println(queue.dequeue()) // prints 2
  println(queue.dequeue()) // prints 3
}