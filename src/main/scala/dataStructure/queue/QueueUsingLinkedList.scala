package dataStructure.queue

class QueueUsingLinkedList[T] {
  private case class Node(var value: T, var next: Node)

  private var head: Node = _
  private var tail: Node = _

  def enqueue(value: T): Unit = {
    val newNode = Node(value, null)
    if (tail == null) {
      head = newNode
      tail = newNode
    } else {
      tail.next = newNode
      tail = newNode
    }
  }

  def dequeue(): Option[T] = {
    if (head == null) {
      None
    } else {
      val value = head.value
      head = head.next
      if (head == null) {
        tail = null
      }
      Some(value)
    }
  }

  def front(): Option[T] = {
    if (head == null) {
      None
    } else {
      Some(head.value)
    }
  }

  def rear(): Option[T] = {
    if (tail == null) {
      None
    } else {
      Some(tail.value)
    }
  }
  def isEmpty(): Boolean = {
    head == null
  }

  def printElements(node: Node): Unit = {
    if(node == null) println()
    else{
      print(node.value + " ")
      printElements(node.next)
    }
  }

}

object QueueUsingLinkedList extends App{
  val queue = new QueueUsingLinkedList[Int]
  queue.enqueue(1)
  queue.enqueue(2)
  queue.enqueue(3)
  queue.printElements(queue.head)
  println(queue.dequeue()) // prints 1
  println(queue.dequeue()) // prints 2
  println(queue.dequeue()) // prints 3
}

