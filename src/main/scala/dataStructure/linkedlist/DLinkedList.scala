package dataStructure.linkedlist

case class node[T](var value: T, var prev: node[T] = null, var next: node[T] = null)
class DLinkedList[T] {
  var head: node[T] = null
  private var size = 0

  def isEmpty: Boolean = head == null

  def display(): Unit = {
    var temp = head
    while(temp != null){
      print(temp.value + " <-> ")
      temp = temp.next
    }
    println(null)
  }

  def list_length(node: node[T]): Int = {
    var temp = node
    var count = 0
    while(temp != null){
      count += 1
      temp = temp.next
    }
    count
  }

  def insertAtEnd(value: T): Unit = {
     val newNode = node(value)
     if(head == null) head = newNode
     else{
       var temp = head
       while(temp.next != null) temp = temp.next
       temp.next = newNode
       newNode.prev = temp
     }
    size += 1
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
  // 1 2 3    4    5
  def insertAfter(nod: node[T], value: T): Unit = {
    val newNode = node(value)
    if(head == null || node == null) println("Empty list or null node")
    else{
      newNode.next = nod.next
      nod.next = newNode
      newNode.prev = nod
      if(newNode.next != null) newNode.next.prev = newNode
    }
    size += 1
  }

  // 1 2 3 4 5
  def insertBefore(nod: node[T], value: T): Unit = {
    val newNode = node(value)
    if(nod == null ) println("The Given Node is null")
    else {
      newNode.prev = nod.prev
      nod.prev = newNode
      newNode.next = nod
      if(newNode.prev != null){
        newNode.prev.next = newNode
      }
      else head = newNode
    }
    size += 1
  }

  def insertAtParticularPosition(value: T, position: Int): Unit = {
    val newNode = node(value)
    var pos = position
    if (head == null) println("Empty list is given")
    else {
      if (pos < 1 || pos > size) println("Invalid Position !")
      else if (pos == 1) insertAtBeginning(value)
      else {
        var temp = head
        var i = 0
        while (i < position - 1) {
          temp = temp.next
          i += 1
        }
        // Insert the new node
        newNode.next = temp.next
        newNode.prev = temp
        temp.next = newNode
      }
    }
    size += 1
  }

  // 1 2 3 4 5
  def remove(value: T): Unit = {
    var temp = head
    if (head == null) println("Empty List is found")
    else if (head.value == value) {
      head = head.next
      size -= 1
    }
    // 1 2 3 4 5
    else {
      while (temp.next != null) {
        if (temp.next.value == value) {
          temp.next = temp.next.next
          temp.next.prev = temp
        }
        temp = temp.next
      }
      size -= 1
    }
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
  list_1.display()
  println(list_1.list_length(list_1.head))
  println()
  println()

  private val list_2 = new DLinkedList[Int]
  println(list_1.isEmpty)
  list_2.insertAtBeginning(10)
  list_2.insertAtEnd(9)
  list_2.insertAtParticularPosition(8, 2)
  list_2.display()
  list_2.insertAtEnd(5)
  list_2.insertAfter(list_2.head.next.next,6)
  list_2.insertAtParticularPosition(7, 3)
  list_2.display()
  list_2.display()
  list_2.insertAtEnd(5)
  list_2.insertBefore(list_2.head.next, 10)
  list_2.display()
  list_2.remove(6)
  list_2.display()
  list_2.remove(6)
  list_2.display()
  list_2.removeAt(0)
  list_2.display()
//  list_1.removeAllOccurrences(5)
//  list_1.display()


}
