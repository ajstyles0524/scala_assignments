package dataStructure.linkedlist

case class nod[T](var value: T, var next: nod[T] = null)
class CLinkedList[T] {
  var head: nod[T] = null
  var size = 0

  def isEmpty: Boolean = head == null

  def display(): Unit = {
    if (head == null) {
      println("The list is empty.")
      return
    }
    var curr = head
    do {
      print(curr.value + " ")
      curr = curr.next
    } while (curr != head)
    println()
  }


  def insertAtLast(data: T): Unit = {
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
    }
    size += 1
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
    size += 1
  }

  // 1 2 3   4   5
  def insertAfter(nodee: nod[T], value: T): Unit = {
    val newNode = nod(value)
    if (head == null || node == null) println("Empty list or null node")
    else {
      newNode.next = nodee.next
      nodee.next = newNode
    }
    size += 1
  }

  // 1 2 3   4   5
  def insertBefore(nodee: nod[T], value: T): Unit = {
    val newNode = nod(value)
    if (nod == null) println("The Given Node is null")
    else {
      if (nodee == head) {
        newNode.next = head
        head = newNode
      }
      else {
        var p: nod[T] = null
        var temp: nod[T] = head
        while (temp != nodee) {
          p = temp
          temp = temp.next
        }
        newNode.next = p.next
        p.next = newNode
      }
    }
    size += 1
  }

  def insertAtParticularPosition(value: T, position: Int): Unit = {
    val newNode = nod(value)
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
        temp.next = newNode
      }
    }
    size += 1
  }

  def remove(value: T): Unit = {
    if (head == null) println("Empty List is found")
    else if (head.value == value) {
      head = head.next
      size -= 1
    }
    // 1 2 3 2 5
    else {
      var temp = head
      while (temp.next != head) {
        if (temp.next.value == value) {
          temp.next = temp.next.next
        }
        temp = temp.next
      }
      size -= 1
    }
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
  list_1.display()
  println()
  println()


  private val list_2 = new CLinkedList[Int]
  println(list_2.isEmpty)
  list_2.insertAtBeginning(10) // 10
  list_2.insertAtLast(9) // 10 9
  list_2.insertAtParticularPosition(8, 2) // 10 9 8
  list_2.display()
  list_2.insertAtLast(5) // 10 9 8 5
  list_2.insertAfter(list_2.head.next.next, 6)
  list_2.insertAtParticularPosition(7, 3)
  list_2.display()
  list_2.display()

  list_2.insertBefore(list_2.head.next, 10)
  list_2.display()
  list_2.remove(6)
  list_2.display()
  list_2.deleteAtIndex(0)
  list_2.display()
}

