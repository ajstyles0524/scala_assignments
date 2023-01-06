package dataStructure.queue

class QueueUsingLinkedList[T] {
  private case class Node(var data: T, var next: Node)

  private var head: Node = _

  def enqueue(item: T): Unit = {
    val newNode = Node(item, null)
    if (isEmpty()) {
      head = newNode
    } else {
      var curr = head
      while (curr.next != null) {
        curr = curr.next
      }
      curr.next = newNode
    }
  }

  def dequeue(): T = {
    if (isEmpty()) {
      throw new Exception("Queue is empty")
    } else {
      val item = head.data
      head = head.next
      item
    }
  }

  def isEmpty(): Boolean = {
    head == null
  }
}

object QueueUsingLinkedList extends App{
  val queue = new QueueUsingLinkedList[Int]
  queue.enqueue(1)
  queue.enqueue(2)
  queue.enqueue(3)
  println(queue.dequeue()) // prints 1
  println(queue.dequeue()) // prints 2
  println(queue.dequeue()) // prints 3
}

