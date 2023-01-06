package dataStructure.linkedlist

import scala.annotation.tailrec

case class node[T](var value: T, var prev: node[T] = null, var next: node[T] = null)
class DLinkedList[T] {
  var head: node[T] = null
  private var size = 0

  def isEmpty: Boolean = head == null

  def printList(): Unit = {
    @tailrec
    def printRec(node: node[T]): Unit={
      if(node == null) println(null)
      else {
        print(node.value + " <-> ")
        printRec(node.next)
      }
    }
    printRec(head)
  }

  def list_length(node: node[T]): Int={
    @tailrec
    def list_lengthRec(node: node[T], acc:Int):Int ={
      if(node == null) acc
      else list_lengthRec(node.next,acc+1)
    }
    list_lengthRec(node,acc=0)
  }

  def insertAtEnd(value: T): Unit ={
    val newNode = node(value)
    @tailrec
    def insertHelper(node: node[T]): Unit ={
      if(node.next == null){
        node.next = newNode
        newNode.prev = node
        size += 1
      }
      else insertHelper(node.next)
    }
    if(head == null) {
      head = newNode
      size += 1
    }
    else insertHelper(head)
  }

  def insertAtBeginning(value: T): Unit = {
    val newNode = node(value)
    if(head == null) head = newNode
    else{
      newNode.next = head
      head.prev = newNode
      head = newNode
    }
    size += 1
  }

 // 1 2 3 5
  def insertAfter(nod:node[T],value:T): Unit ={
    val newNode = node(value)
    @tailrec
    def insertAfterHelper(node: node[T], nod: node[T]):Unit ={
      if(node == nod){
        newNode.next = nod.next
        nod.next = newNode
        newNode.prev = nod
        size += 1
        if(newNode.next != null) newNode.next.prev = newNode
      }
      else insertAfterHelper(node.next,nod)
    }
    if(head == null || node == null) println("Empty list or null node")
    else insertAfterHelper(head,nod)
  }

  def insertBefore(nod:node[T], value: T): Unit ={
    val newNode = node(value)
    @tailrec
    def insertBeforeHelper(node: node[T], nod: node[T]): Unit ={
      if(node == nod){
        newNode.prev = nod.prev
        nod.prev = newNode
        newNode.next = nod
        size += 1
        if(newNode.prev != null){
          newNode.prev.next = newNode
        }
        else head = newNode
      }
      else insertBeforeHelper(node.next, nod)
    }
    if(head == null || node == null) println(" Empty list or null node")
    else insertBeforeHelper(head, nod)
  }

  def insertAtParticularPosition(value: T, position: Int): Unit ={
    val newNode = node(value)
    @tailrec
    def insertAtPositionRec(node: node[T], position: Int): Unit = {
      if(position ==1){
        newNode.next = node.next
        newNode.prev = node
        node.next = newNode
      }
      else insertAtPositionRec(node.next,position-1)
    }
    if(head == null && position == 0){
      head = newNode
      size += 1
    }
    else if(head != null && position == 0){
      insertAtBeginning(value)
    }
    else if(position < 0 || position > list_length(head)) println("Invalid Position !")
    else insertAtPositionRec(head,position)
  }

  def removeValue(value: T): Unit ={
    val prev:node[T] = null
    @tailrec
    def removeValHelper(previous: node[T], node: node[T], value:T):Unit ={
      var prev = previous
      if(node == null) println("Element is not Present")
      else if(node.value == value){
        if(prev == null && node == head){
          head = head.next
          size -= 1
        }
        else{
          prev.next = node.next
          node.prev = prev
          size -= 1
        }
      }
      else{
        prev = node
        removeValHelper(prev,node.next,value)
      }
    }
    removeValHelper(prev, head, value)
  }

  def removeAt(index: Int): Unit = {
    if (index < 0 || index >= size) println("Invalid Position !")
    else if (index == 0) {
      head = head.next
      size -= 1
    }
    else {
      // Find the node at the desired position
      var temp = head
      var i = 0
      while (i < index - 1) {
        temp = temp.next
        i += 1
      }
      // Remove the node by updating the pointers
      temp.next = temp.next.next
      temp.next.prev = temp
    }
    size -= 1
  }

  def removeAtIndex(index: Int): Unit ={
    val prev:node[T] = null
    @tailrec
    def removeAtIndexHelper(previous:node[T], node: node[T], index: Int): Unit ={
      var prev = previous
      if(index == 0){
        if(prev == null && node == head){
          head = head.next
          size -= 1
        }
        else{
          prev.next = node.next
          node.prev = prev
          size -= 1
        }
      }
      else{
        prev = node
        removeAtIndexHelper(prev,node.next,index-1)
      }
    }

    if(index < 0 || index >= list_length(head)) println("Invalid Position !")
    else removeAtIndexHelper(prev, head, index)
  }

  def removeAllOccurrences(value: T, node: node[T]): Unit = {
    var temp = node
    while (temp != null && temp.value == value) {
      head = temp.next
      temp = head
    }
    // 1 2 3 4 5
    while (temp != null && temp.next != null) {
      if (temp.next.value == value) {
        temp.next.prev = temp
        temp.next = temp.next.next
      }
      else temp = temp.next
    }
  }


  def removeAll(value: T): Unit = {
    def removeAllHelper(node: node[T], value: T): node[T] = {
      if (node == null) null
      else if (node.value == value) removeAllHelper(node.next, value)
      else {
        node.next = removeAllHelper(node.next, value)
        node
      }
    }
    head = removeAllHelper(head, value)
  }

 // 1 2 3 4 5
 //
 // 5 4 3 2 1
  def reverseUsingIteration(node:node[T]):Unit ={
    var curr:node[T] = node
    var prev:node[T] = null
    var nxt: node[T] = null
    while (curr != null) {
      nxt = curr.next
      curr.next = prev
      curr.prev = nxt
      prev = curr
      curr = nxt
    }
   head = prev
  }

  def reverseUsingRec(node: node[T]): node[T] = {
    if (node == null || node.next == null)  node
    else{
      val reversed = reverseUsingRec(node.next)
      node.next.next = node
      node.prev = node.next
      node.next = null
      reversed
    }
  }
}


object DList extends App {
  private val list_1 = new DLinkedList[Int]
  println(list_1.isEmpty)
  private val first = node(1)
  private val second = node(2)
  private val third = node(3)
  private val fourth = node(4)
  private val fifth = node(5)
  list_1.head = first
  first.next = second
  second.prev = first
  second.next = third
  third.prev = second
  third.next = fourth
  fourth.prev = third
  fourth.next = fifth
  fifth.prev = fourth
  list_1.printList()
  println(list_1.list_length(list_1.head))
  println()


  private val list_2 = new DLinkedList[Int]
  println(list_1.isEmpty)
  list_2.insertAtBeginning(10)
  list_2.insertAtEnd(8)
  list_2.insertAfter(list_2.head,8)
  list_2.insertAfter(list_2.head.next.next,7)
  list_2.insertBefore(list_2.head.next,80)
  list_2.printList()
  list_2.insertAtParticularPosition(10,1)
  list_2.removeValue(800)
  //list_2.removeAtIndex(5)
  list_2.printList()
  //list_2.reverseUsingIteration(list_2.head)
  list_2.reverseUsingRec(list_2.head)
  //list_2.removeAll(8)
  //list_2.removeAllOccurrences(10,list_2.head)
  list_2.printList()
}
