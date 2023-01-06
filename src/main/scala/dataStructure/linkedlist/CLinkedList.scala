package dataStructure.linkedlist

import scala.annotation.tailrec

case class nod[T](var value: T, var next: nod[T] = null)
class CLinkedList[T] {
  var head: nod[T] = null

  def isEmpty: Boolean = head == null

  def list_length(nodee: nod[T]):Int  ={
    @tailrec
    def list_lengthHelper(nodee: nod[T], acc:Int = 1):Int={
      if(nodee == head) acc
      else list_lengthHelper(nodee.next,acc+1)
    }
    list_lengthHelper(nodee.next)
  }

  def search(value: T): Unit ={
    @tailrec
    def searchHelper(nodee: nod[T], index: Int = 1): Unit={
      if(nodee == head) println("Element is not Present")
      else if(nodee.value == value) println(s"$value is present at $index")
      else searchHelper(nodee.next,index+1)
    }
    if(head.value == value) println(s"$value is present at 0 index")
    else searchHelper(head.next)
  }

  def printList():Unit = {
    @tailrec
    def printListRec(nodee: nod[T]):Unit={
      if(nodee == head ) println()
      else{
        print(nodee.value + " ")
        printListRec(nodee.next)
      }
    }
    if(head == null) println("Empty list")
    else {
      print(head.value + " ")
      printListRec(head.next)
    }
  }


  def insertAtBeginning(data: T): Unit = {
    val newNode = nod(data)
    if (head == null) {
      head = newNode
      newNode.next = newNode
    } else {
      var curr = head
      while (curr.next != head) {
        curr = curr.next
      }
      curr.next = newNode
      newNode.next = head
      head = newNode
    }
  }

  def insertAtEnd(data: T): Unit ={
    val newNode = nod(data)
    @tailrec
    def insertAfterRec(nodee: nod[T]): Unit ={
      if(nodee.next == head){
        nodee.next = newNode
        newNode.next = head
      }
      else insertAfterRec(nodee.next)
    }
    if(head == null){
      head = newNode
      newNode.next = newNode
    }
    else insertAfterRec(head.next)
  }

  def insertAfter(nodee:nod[T], value:T):Unit ={
    val newNode = nod(value)
    @tailrec
    def insertAfterRec(nodee: nod[T], node: nod[T]): Unit ={
      if(nodee == node){
        newNode.next = node.next
        node.next = newNode
      }
      else insertAfterRec(nodee, node.next)
    }
    if(head == null || nodee == null) println("Empty list or null node")
    else if(nodee == head){
      newNode.next = nodee.next
      head.next = nodee
    }
    else insertAfterRec(nodee,head.next)
  }

  // 1 2 3  4   5
  def insertBefore(nodee: nod[T], value: T): Unit ={
    val newNode = nod(value)
    @tailrec
    def insertBeforeHelper(nodee:nod[T], node: nod[T]): Unit ={
      if(node.next == nodee){
        node.next = newNode
        newNode.next = nodee
      }
      else {
        insertBeforeHelper(nodee, node.next)
      }
    }
    if(nodee == null || head == null) println(" Empty list or null node")
    else if(nodee == head){
      newNode.next = head
      head = newNode
    }
    else insertBeforeHelper(nodee,head.next)
  }

  def insertAtParticularPosition(value: T, pos: Int): Unit = {
    val newNode = nod(value)
    @tailrec
    def insertAtParticularPositionRec(node: nod[T], value: T, pos: Int): Unit = {
      if (pos == 1) {
        newNode.next = node.next
        node.next = newNode

      } else insertAtParticularPositionRec(node.next, value, pos - 1)
    }

    if (head == null && pos == 0) {
      head = newNode

    }
    else if (head != null && pos == 0) {
      insertAtBeginning(value)
    }
    else if (pos < 0 || pos > list_length(head)) println("Invalid Position !")
    else insertAtParticularPositionRec(head, value, pos)
  }


  // 1 2 3 4 5
  def remove(value: T): Unit = {
    val prev: nod[T] = head
    @tailrec def removeRec(previous: nod[T], node: nod[T], value: T): Unit = {
      var prev = previous
      if (node == null) println("Element is not found")
      else if (node.value == value) {
          prev.next = node.next
      }
      else {
        prev = node
        removeRec(prev, node.next, value)
      }
    }
    if(head.value == value) head = head.next
    else removeRec(prev, head.next, value)
  }




  def deleteAtIndex(index: Int): Unit = {
    if (head == null) {
      return
    }
    if (index == 0) {
      if (head.next == head) {
        head = null
      } else {
        var curr = head
        while (curr.next != head) {
          curr = curr.next
        }
        curr.next = head.next
        head = head.next
      }
    } else {
      var curr = head
      var prev = head
      var count = 0
      while (count < index && curr.next != head) {
        prev = curr
        curr = curr.next
        count += 1
      }
      if (count == index) {
        prev.next = curr.next
      }
    }
  }


  def removeAt(index: Int): Unit = {
    val prev: nod[T] = head
    @tailrec
    def removeAtRec(prev: nod[T], node: nod[T], index: Int): Unit = {
      var previous = prev
      if (index == 1) {
        if (previous == null && node == head) {
          head = head.next
        }
        else {
          previous.next = node.next
        }
      }
      else {
        previous = node
        removeAtRec(previous, node.next, index - 1)
      }
    }
    if (head == null) println("Empty list found !")
    else if (index < 0 || index >= list_length(head)) println("Invalid Index !")
    else if(index == 0) head = head.next
    else removeAtRec(prev, head.next, index)
  }


}

object CList extends App {

  private val list_1 = new CLinkedList[Int]
  println(list_1.isEmpty)
  private val first = nod(1)
  private val second = nod(2)
  private val third = nod(3)
  private val fourth = nod(4)
  private val fifth = nod(5)
  list_1.head = first
  first.next = second
  second.next = third
  third.next = fourth
  fourth.next = fifth
  fifth.next = first
  list_1.printList()
  println(list_1.list_length(list_1.head))
  list_1.search(4)
  println()
  println()


  private val list_2 = new CLinkedList[Int]
  println(list_2.isEmpty)
  list_2.insertAtEnd(9)
  list_2.insertAtBeginning(10) // 10
  list_2.insertAtEnd(9) // 10 9
  list_2.insertAfter(list_2.head.next,8)
  list_2.insertBefore(list_2.head.next,11)
  list_2.insertAtParticularPosition(12,2)
  list_2.printList()
  list_2.removeAt(3)
  list_2.printList()
}

