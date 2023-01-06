package dataStructure.queue

// Queue is a linear data structure that follows a particular order in which
// the operations are performed for storing data. The order is First In First Out (FIFO).
// The difference between stacks and queues is in removing.
// In a stack we remove the item the most recently added;
// in a queue, we remove the item the least recently added.

//Queue is used when things don’t have to be processed immediately,
// but have to be processed in First In Firs Out order like Breadth First Search.
// This property of Queue makes it also useful in following kind of scenarios.
//
//When a resource is shared among multiple consumers. Examples include CPU scheduling, Disk Scheduling.
//When data is transferred asynchronously between two processes

class QueueUsingArray (val capacity: Int) {
  private var arr = new Array[Int](capacity)
  private var front = 0
  private var rear = -1
  private var numItems = 0

  def enqueue(item: Int): Unit = {
    if (isFull()) {
      throw new Exception("Queue is full")
    } else {
      rear += 1
      if (rear == capacity) {
        rear = 0
      }
      arr(rear) = item
      numItems += 1
    }
  }

  def dequeue(): Int = {
    if (isEmpty()) {
      throw new Exception("Queue is empty")
    } else {
      val item = arr(front)
      front += 1
      if (front == capacity) {
        front = 0
      }
      numItems -= 1
      item
    }
  }

  def isFull(): Boolean = {
    numItems == capacity
  }

  def isEmpty(): Boolean = {
    numItems == 0
  }

  def size(): Int = {
    numItems
  }

  def frontVal(): Int ={
    if(isEmpty()) throw new Exception("Queue is empty")
    else arr(front)
  }

  def rearVal(): Int ={
    if(isFull()) throw new Exception("Queue is full")
    else arr(rear)
  }
}


object Queue extends App{
  val queue = new QueueUsingArray(5)
  queue.enqueue(1)
  queue.enqueue(2)
  queue.enqueue(3)
  println(queue.frontVal())
  println(queue.rearVal())
  println(queue.dequeue()) // prints 1
  println(queue.dequeue()) // prints 2
  println(queue.dequeue()) // prints 3
}