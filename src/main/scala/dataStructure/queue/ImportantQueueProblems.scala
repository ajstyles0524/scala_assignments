package dataStructure.queue

import scala.collection.mutable

object ImportantQueueProblems {
  private def reverseQueue[T](queue: mutable.Queue[T]): mutable.Queue[T] = {
    if (queue.isEmpty) {
      queue
    } else {
      val element = queue.dequeue()
      reverseQueue(queue).enqueue(element)
    }
  }

  def main(args: Array[String]): Unit ={
    val queue = mutable.Queue(1, 2, 3, 4, 5)
    println(queue)
    val reversedQueue = reverseQueue(queue)
    println(reversedQueue)
  }
}
