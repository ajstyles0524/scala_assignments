package dataStructure.linkedlist
import scala.annotation.tailrec

case class Node[T](var value: T, var next: Node[T] = null)
class SLinkedList[T] {
  var head: Node[T] = null
  private var size = 0

  def isEmpty: Boolean = head == null

  def printRecursive(node: Node[T]): Unit = {
      if (node != null) {
        print(node.value + "->")
        printRecursive(node.next)
    }
      else println(null)
  }

  def insertAtBeginning(value: T): Unit = {
    val newNode = Node(value)
    if (head == null) head = newNode
    else {
      newNode.next = head
      head = newNode
    }
    size += 1
  }

  def insertAtEnd(a: T): Unit = {
    val newNode = Node(a, null)
    @tailrec
    def addRecursive(node: Node[T]): Unit = {
      if (node.next == null) {
        node.next = newNode
        size += 1
      } else {
        addRecursive(node.next)
      }
    }
    if (head == null) {
      head = newNode
      size += 1
    } else {
      addRecursive(head)
    }
  }

  def insertAfter(node: Node[T], newValue: T): Unit = {
    val newNode = Node(newValue)
    @tailrec
    def insertAfterRec(node: Node[T], newValue: T): Unit = {
      val temp = head
      if(node == temp){
        newNode.next = temp.next
        temp.next = newNode
        size += 1
    } else insertAfterRec(node.next, newValue)
  }
    if (head == null) {
      head = newNode
      size += 1
    }
    else insertAfterRec(node, newValue)
  }


  def insertAtParticularPosition(node: Node[T], value: T, pos: Int): Unit = {
    val newNode = Node(value)
    @tailrec
    def insertAtParticularPositionRec(node: Node[T], value: T, pos: Int): Unit = {
      if (pos == 1) {
        newNode.next = node.next
        node.next = newNode
        size += 1
      } else insertAtParticularPositionRec(node.next, value, pos - 1)
    }
    if (head == null && pos == 1) head = newNode
    else if (pos < 1 || pos > size) println("Invalid Position !")
    else insertAtParticularPositionRec(node,value,pos)
    }


 // 1 2 3 4 5
  def remove (value: T): Unit = {
    val prev = null
    val node = head
    @tailrec
    def removeRec(previous: Node[T] = null, node: Node[T], value: T): Unit = {
      var prev = previous
      if (node.value == value) {
        prev.next = node.next
        size -= 1
      }
      else {
        prev = node
        removeRec(prev, node.next, value)
      }
    }
    if (node == null) println("Empty List is found")
    else removeRec(prev, node, value)
  }


  def list_size(): Int = {
    size
  }

  def removeAt(index: Int): Unit = {
    val prev = null
    val node = head
    @tailrec
    def removeAtRec(prev: Node[T], node: Node[T], index: Int): Unit = {
      var previous = prev
      if (index == 1) {
        if (previous == null && node == head) {
          head = head.next
          size -= 1
        }
        else {
          previous.next = node.next
          size -= 1
        }
      }
      else {
        previous = node
        removeAtRec(previous, node.next, index - 1)
      }
    }
    if(head == null) println("Empty list found !")
    else if(index < 0 || index > size) println("Invalid Index !")
    else removeAtRec(prev, node, index)
  }

  def removeAtBeginning(): Unit = {
    removeAt(1)
  }

  def removeAtEnd(): Unit = {
    removeAt(size)
  }

  def removeAllOccurrences(value: T): Unit = {
    if (head == null) println("Empty List is found")
    else if (head.value == value) {
      head = head.next
      size -= 1
    }
    else {
      var curr = head
      var prev: Node[T] = null
      while (curr != null) {
        if (curr.value == value) prev.next = curr.next
        else prev = curr
        curr = curr.next
      }
    }
  }
}

object List extends App{
  private val list_1 = new SLinkedList[Int]
  println(list_1.isEmpty)
  list_1.insertAtBeginning(10)
  list_1.insertAtEnd(8)
  list_1.insertAfter(list_1.head,9)
  list_1.insertAtParticularPosition(list_1.head,7,3)
  list_1.printRecursive(list_1.head)
  list_1.remove(9)
  list_1.removeAt(2)
  list_1.printRecursive(list_1.head)
}

