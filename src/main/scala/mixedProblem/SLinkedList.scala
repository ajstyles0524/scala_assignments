package mixedProblem
import scala.annotation.tailrec
import scala.util.control.Breaks.{break, breakable}

case class Node[T](var value: T, var next: Node[T] = null)
class SLinkedList[T] {
  var head: Node[T] = null
  private var size = 0

  def isEmpty: Boolean = head == null

  def display(): Unit = {
    var temp = head
    while (temp != null) {
      print(temp.value + " -> ")
      temp = temp.next
    }
    println("null")
  }

  def list_length(node: Node[T]): Int = {
    @tailrec def list_lengthHelper(node: Node[T], acc: Int): Int = {
      if (node == null) acc
      else list_lengthHelper(node.next, acc + 1)
    }
    list_lengthHelper(node, acc = 0)
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

  def insertAtEnd(value: T): Unit = {
    val newNode = Node(value)
    if (head == null) head = newNode
    else {
      var temp = head
      while (temp.next != null) temp = temp.next
      temp.next = newNode
    }
    size += 1
  }

  // 10 11 12 13 15

  def insertInBetween(value: T, node: Node[T]): Unit = {
    val newNode = Node(value)
    if (head == null) head = newNode
    else {
      if (node == null) println("Node is not Present in the list")
      newNode.next = node.next
      node.next = newNode
    }
    size += 1
  }

  def insertAtParticularPosition(value: T, position: Int): Unit = {
    val newNode = Node(value)
    val pos = position
    if (head == null && position == 1) head = newNode
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
      while (temp.next != null) {
        if (temp.next.value == value) {
          temp.next = temp.next.next
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
    }
    size -= 1
  }

  def removeAllOccurrences(value: T): Unit = {
    if (head == null) println("Empty List is found")
    else {
      var temp = head
      var prev: Node[T] = null
      while (temp != null) {
        if (temp.value == value) {
          prev.next = temp.next
          size -= 1
        } else prev = temp
        temp = temp.next
      }
    }
  }

  def contains(value: T): Boolean = {
    var temp = head
    while (temp != null) {
      if (temp.value == value) return true
      temp = temp.next
    }
    false
  }

  def midElement(node: Node[T]): Option[T] = {
    if (head == null || head.next == null) None
    var minIdx = list_length(node) / 2
    var temp = node
    while (minIdx != 0) {
      temp = temp.next
      minIdx -= 1
    }
    Some(temp.value)
  }

  def midElement_2(node: Node[T]): Option[T] = {
    if (head == null || head.next == null) None
    var slow = head
    var fast = head
    while (fast != null && fast.next != null) {
      slow = slow.next
      fast = fast.next.next
    }
    Some(slow.value)
  }

  def deleteMid(node: Node[T]): Unit = {
    if (head == null || head.next == null) println("We can't find middle")
    else {
      var nod: Node[T] = null
      var slow = head
      var fast = head
      while (fast != null && fast.next != null) {
        nod = slow
        slow = slow.next
        fast = fast.next.next
      }
      nod.next = slow.next
    }
  }

  // 1 2 3 4 5
  def detectLoop(node: Node[T]): Boolean = {
    if (head == null || head.next == null) false
    else {
      var slow = head
      var fast = head
      while (fast != null && fast.next != null) {
        slow = slow.next
        fast = fast.next.next
        if (slow == fast) return true
      }
      false
    }
  }
  // 1 2 3 4 5
  def reverseLinkedList(node: Node[T]): Unit = {
    if (head == null || head.next == null) head
    else {
      var prev: Node[T] = null
      var curr: Node[T] = node
      var nxt: Node[T] = null
      while (curr != null) {
        nxt = curr.next
        curr.next = prev
        prev = curr
        curr = nxt
      }
      head = prev
    }
  }

  // using recursion
  // 1 2 3 4
  // reversed = reverseLinkedList(reversed= reverseLinkedList(reversed = reverseLinkedList(reversed= reverseLinkedList(4))
  def reverseLinkedList_2(node: Node[T]): Node[T] = {
    if (node == null || node.next == null) node
    else {
      val reversed = reverseLinkedList_2(node.next)
      node.next.next = node
      node.next = null
      reversed
    }
  }

  // using stack
  def reverseLinkedList_3(node: Node[T]): Unit = {
    ???
  }

  // using tail-recursive
  def reverseLinkedList_4(node: Node[T]): Node[T] = {
    @tailrec
    def reverseTailRecursive(result: Node[T], current: Node[T]): Node[T] = {
      if (current.next == null) {
        current.next = result
        current
      } else {
        val next = current.next
        current.next = result
        reverseTailRecursive(current, next)
      }
    }
    reverseTailRecursive(null, node)
  }



  def removeLoop(node: Node[T]): Unit = {
    if (head == null || head.next == null) println("Loop is not detected")
    else {
        var slow = head
        var fast = head
        breakable{
        while (fast != null && fast.next != null) {
          slow = slow.next
          fast = fast.next.next
          if (slow == fast)
              break
         }
       }
      // 1 2 3 4 5
        if (slow == head) {
          while (slow.next != head) {
            slow = slow.next
          }
          slow.next = null
          }
        else if (slow == fast) {
          slow = head
          while (slow.next != fast.next) {
            slow = slow.next
            fast = fast.next
          }
          fast.next = null
        }
     }
  }

  def getNthFromFront(node: Node[T], n: Int): Option[T] = {
    var temp = node
    var count = 1
    while(temp != null){
      if (count == n) return Some(temp.value)
      count += 1
      temp = temp.next
    }
    None
  }

  def getNthFromLast(node: Node[T], n: Int): Option[T]  = {
    var temp = node
    var count = 0
    while (head != null) {
      count += 1
      head = head.next
    }
    if (n > count) None
    else {
      var i = 0
      while (i < count-n) {
        temp = temp.next
        i += 1
      }
      Some(temp.value)
    }
  }

  def removeNthElementFromLast(node: Node[T], n: Int): Unit = {
    val l = list_length(node) - n
    if (l == 0) {
      head.next = null
      head = null
    }
    else {
      // Find the node at the desired position
      var temp = head
      var i = 0
      while (i < l - 1) {
        temp = temp.next
        i += 1
      }
      // Remove the node by updating the pointers
      temp.next = temp.next.next
    }
  }


}


object List extends App{
  private val list_1 = new SLinkedList[Int]
  println(list_1.isEmpty)
  list_1.insertAtBeginning(15)
  list_1.insertAtBeginning(13)
  list_1.insertAtBeginning(12)
  list_1.insertAtBeginning(11)
  list_1.insertAtBeginning(10)
  //list_1.insertInBetween(list_1.head.next.next,14)
  list_1.display()
//  list_1.insertAtEnd(9)
//  list_1.insertAtParticularPosition(8,2)
//  list_1.insertAtEnd(5)
//  list_1.insertInBetween(6, list_1.head.next.next)
//  list_1.insertAtParticularPosition(7,3)
//  list_1.display()
//  list_1.insertAtEnd(5)
//  list_1.display()
//  list_1.remove(6)
//  list_1.display()
//  list_1.removeAllOccurrences(5)
//  list_1.display()
//  println(list_1.getNthFromFront(list_1.head, 2))
//  list_1.display()
//  list_1.removeNthElementFromLast(list_1.head,2)
//  list_1.display()
//  println()
//  println()




  private var list2 = new SLinkedList[Int]
  private val first = Node(1)
  private val second = Node(2)
  private val third = Node(3)
  private val fourth = Node(4)
  private val fifth = Node(5)
  list2.head = first
  first.next = second
  second.next = third
  third.next = fourth
  fourth.next = fifth
  list2.display()
  println(list2.list_length(list2.head))
  println(list2.midElement(list2.head))
  println(list2.midElement_2(list2.head))
  //list2.deleteMid(list2.head)
  //list2.display()
  //list2.reverseLinkedList(list2.head)
  list2.display()
  list2.head = list2.reverseLinkedList_2(list2.head)
  list2.display()
  list2.head = list2.reverseLinkedList_4(list2.head)
  list2.display()
  println(list2.getNthFromLast(list2.head,2))
  println()
  println()


  private var list3 = new SLinkedList[Int]
  private val e1 = Node(1)
  private val e2 = Node(2)
  private val e3 = Node(3)
  private val e4 = Node(2)
  private val e5 = Node(1)
  list3.head = e1
  e1.next = e2
  e2.next = e3
  e3.next = e4
  e4.next = e5

  //list3.display()

  println(list3.detectLoop(list3.head))
  list3.removeLoop(list3.head)
  list3.display()


}

