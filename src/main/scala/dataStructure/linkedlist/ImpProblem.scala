package dataStructure.linkedlist
import scala.annotation.tailrec
import scala.util.control.Breaks.{break, breakable}

case class Node[T](var value: T, var next: Node[T] = null)

class SLinkedList[T] {

  var head: Node[T] = null
  private var size = 0

  def list_size(): Int = {
    size
  }

  def list_length(node: Node[T]): Int = {
    @tailrec def list_lengthHelper(node: Node[T], acc: Int): Int = {
      if (node == null) acc
      else list_lengthHelper(node.next, acc + 1)
    }
    list_lengthHelper(node, acc = 0)
  }

  def isEmpty: Boolean = head == null

  def search(value: T): Unit = {
    @tailrec
    def searchRecursive(node: Node[T], acc: Int): Unit = {
      if (node == null) println("Element is not Present")
      else if (node.value == value) println(s"Element is present at $acc index")
      else searchRecursive(node.next, acc + 1)
    }
    searchRecursive(head, 0)
  }

  def printList(): Unit = {
    @tailrec
    def printRecursive(node: Node[T]): Unit = {
      if (node != null) {
        print(node.value + "->")
        printRecursive(node.next)
      }
      else println(null)
    }
    printRecursive(head)
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
    val newNode = Node(a)
    @tailrec
    def addRecursive(node: Node[T]): Unit = {
      if (node.next == null) {
        node.next = newNode
        size += 1
      }
      else addRecursive(node.next)
    }
    if (head == null) {
      head = newNode
      size += 1
    }
    else addRecursive(head)
  }

  def insertAfter(node: Node[T], newValue: T): Unit = {
    val newNode = Node(newValue)
    val temp = node

    @tailrec
    def insertAfterRec(node: Node[T], newValue: T): Unit = {
      if (node == temp) {
        newNode.next = temp.next
        temp.next = newNode
        size += 1
      }
      else insertAfterRec(node.next, newValue)
    }

    if (head == null) {
      head = newNode
      size += 1
    }
    else insertAfterRec(node, newValue)
  }

  def insertAtParticularPosition(value: T, pos: Int): Unit = {
    val newNode = Node(value)
    @tailrec
    def insertAtParticularPositionRec(node: Node[T], value: T, pos: Int): Unit = {
      if (pos == 1) {
        newNode.next = node.next
        node.next = newNode
        size += 1
      } else insertAtParticularPositionRec(node.next, value, pos - 1)
    }
    if (head == null && pos == 0) {
      head = newNode
      size += 1
    }
    else if (head != null && pos == 0) {
      insertAtBeginning(value)
    }
    else if (pos < 0 || pos > size) println("Invalid Position !")
    else insertAtParticularPositionRec(head, value, pos)
  }


  // 1 2 3 4 5
  def remove(value: T): Unit = {
    val prev:Node[T] = null
    @tailrec def removeRec(previous: Node[T] = null, node: Node[T], value: T): Unit = {
      var prev = previous
      if (node == null) println("Element is not found")
      else if (node.value == value) {
        if (previous == null && node == head) {
          head = head.next
          size -= 1
        }
        else {
          prev.next = node.next
          size -= 1
        }
      }
      else {
        prev = node
        removeRec(prev, node.next, value)
      }
    }
    removeRec(prev, head, value)
  }

  private def removeAt(index: Int): Unit = {
    val prev: Node[T] = null
    @tailrec
    def removeAtRec(prev: Node[T], node: Node[T], index: Int): Unit = {
      var previous = prev
      if (index == 0) {
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

    if (head == null) println("Empty list found !")
    else if (index < 0 || index >= size) println("Invalid Index !")
    else removeAtRec(prev, head, index)
  }

  def removeAtBeginning(): Unit = {
    removeAt(0)
  }

  def removeAtEnd(): Unit = {
    removeAt(size - 1)
  }


  def removeAllOccurrences(value: T, node: Node[T]): Unit = {
    var temp = node
    while(temp != null && temp.value==value) {
      head = temp.next
      temp = head
    }
    while(temp != null && temp.next != null){
      if(temp.next.value == value) temp.next = temp.next.next
      else temp = temp.next
    }
  }


  // remove all occurrences
  def removeAll(value: T): Unit = {
    def removeAllHelper(node: Node[T],value: T):Node[T]={
      if(node == null) null
      else if(node.value == value) removeAllHelper(node.next,value)
      else {
        node.next = removeAllHelper(node.next,value)
        node
      }
    }
    head = removeAllHelper(head, value)
  }

  def midElement(node: Node[T]): Unit = {
    @tailrec
    def midEleRec(node: Node[T], index: Int): Unit = {
      if (index == 0) println(node.value)
      else midEleRec(node.next, index - 1)
    }
    if (node == null || node.next == null) println("Empty list or only one element is found")
    else {
      val midIdx = list_length(node) / 2
      midEleRec(node, midIdx)
    }
  }

  def midEle(node: Node[T]): Unit = {
    @tailrec
    def midEleRec(node: Node[T], slow: Node[T], fast: Node[T]): Unit = {
      if (fast == null || fast.next == null) println(slow.value)
      else midEleRec(node, slow.next, fast.next.next)
    }
    if (node == null || node.next == null) println("Empty list or only one element is found")
    else midEleRec(node, head, head)
  }

  def deleteMid(node: Node[T]): Unit = {
    var prev: Node[T] = null
    @tailrec
    def delMidRec(node: Node[T], slow: Node[T], fast: Node[T]): Unit = {
      if (fast == null || fast.next == null) prev.next = slow.next
      else {
        prev = slow
        delMidRec(node, slow.next, fast.next.next)
      }
    }
    if (node == null || node.next == null) println("Empty list or only one element is found")
    else delMidRec(node, node, node)
  }

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

  @tailrec
  private def hasLoop(head: Node[T], seen: Set[Node[T]] = Set()): Boolean = {
    if (head == null) {
      return false
    }
    if (seen.contains(head)) {
      return true
    }
    hasLoop(head.next, seen + head)
  }

  def removeLoop(node: Node[T]): Unit = {
    if (head == null || head.next == null) println("Loop is not detected")
    else {
      var slow = head
      var fast = head
      breakable {
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

  // 1 2 3 4 5

  def reverseLinkedList(node: Node[T]): Unit = {
    if (head == null || head.next == null) println("No need to reverse")
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
  // 1 2 3 4 5
  // reversed = reverseLinkedList(reversed= reverseLinkedList(reversed = reverseLinkedList(reversed= reverseLinkedList(4))
  def reverseLinkedList_2(node: Node[Int]): Node[Int] = {
    if (node == null || node.next == null) node
    else{
      val reversed = reverseLinkedList_2(node.next)
      node.next.next = node
      node.next = null
      reversed
    }
  }

  // reverse linked list using stack
  def reverseUsingStack(node: Node[T]): Unit = {
    ???
  }

  def getNthFront(node: Node[T],n:Int):Unit = {
    @tailrec
    def getNthRec(node: Node[T], n: Int, count:Int):Unit ={
      if(count == n) println(node.value)
      else getNthRec(node.next,n, count+1)
    }
    if(n < 1 || n > list_length(node)) println("Invalid Position !")
    else getNthRec(node, n, 1)
  }


  // 1 2 3 4 5 6 7 8
  // from back k =2 then front = 8-2
  def getNthLast(node: Node[T], n: Int): Unit = {
    @tailrec
    def getNthLastRec(node: Node[T], m: Int, count: Int): Unit = {
      if(count == m) println(node.value)
      else getNthLastRec(node.next,m,count+1)
    }
    val idx = list_length(node)-n+1
    if(n < 1 || n > list_length(node)) println("Invalid Position !")
    else getNthLastRec(node, idx, 1)
  }

  def removeNthNode(node: Node[T], n: Int): Unit = {
    var prev: Node[T] = null
    @tailrec
    def removeNthFromLast(previous: Node[T], node: Node[T], index: Int): Unit = {
      if(index == 0) {
        previous.next = node.next
      }
      else{
        prev = node
        removeNthFromLast(prev,node.next,index-1)
      }
    }
    if(n < 1 || n > list_length(node)) println("Invalid Position !")
    else{
      val l = list_length(node) - n
      if(l==0) head = node.next
      else removeNthFromLast(prev,node,l)
    }
  }


  def isPalindrome(head: Node[T]): Boolean = {
    // Find the middle of the linked list
    var slow: Node[T] = head
    var fast: Node[T] = head
    while (fast != null && fast.next != null) {
      slow = slow.next
      fast = fast.next.next
    }
    // Reverse the second half of the linked list
    var prev: Node[T] = null
    var curr: Node[T] = slow
    var nxt: Node[T] = null
    while (curr != null) {
      nxt = curr.next
      curr.next = prev
      prev = curr
      curr = nxt
    }

    // Compare the first half of the linked list to the reversed second half
    var p1: Node[T] = head;
    var p2: Node[T] = prev;
    while (p2 != null) {
      if (p1.value != p2.value) return false;
      p1 = p1.next;
      p2 = p2.next;
    }
     true;
  }


  def sortList(head: Node[Int]): Node[Int] = {
    if ((head == null) || (head.next == null)) {
      return head
    }
    var fast = head.next
    var slow = head

    while ((fast != null) && (fast.next != null)) {
      fast = fast.next.next
      slow = slow.next
    }

    val mid = slow.next
    slow.next = null
    val l = sortList(head)
    val r = sortList(mid)
    merge(l, r)
  }

  def merge(l: Node[Int], r: Node[Int]):Node[Int] = {
    if (l == null) return r
    if (r == null) return l
    if (l.value > r.value) {
      r.next = merge(l, r.next)
      r
    } else {
      l.next = merge(l.next, r)
      l
    }
  }


  def sortZeroOneTwo(node: Node[Int]): Node[Int] ={
    var curr = node
    var z = 0
    var o = 0
    var t = 0
    while (curr != null) {
      if (curr.value == 0) z += 1
      else if (curr.value == 1) o += 1
      else t += 1
      curr = curr.next
    }
    var start = node
    while (start != null) {
      if (z > 0) {
        start.value = 0
        z -= 1
      }
      else if (o > 0) {
        start.value = 1
        o -= 1
      }
      else {
        start.value = 2
        t -= 1
      }
      start = start.next
    }
    node
  }






  def oddEvenList(head: Node[T]): Node[T] = {
    if (head == null) return head
    var odd: Node[T] = head
    var evn: Node[T] = head.next
    var dum: Node[T] = head.next
    while (evn != null && evn.next != null) {
      odd.next = odd.next.next
      evn.next = evn.next.next
      odd = odd.next
      evn = evn.next
    }
    odd.next = dum
     head
  }


  def rearrangeEvenOdd(node: Node[Int]): Node[Int] = {
    var even: Node[Int] = null
    var odd: Node[Int] = null
    var oddTail: Node[Int] = null
    var evenTail: Node[Int] = null
    var curr: Node[Int] = node
    var temp: Node[Int] = node

    while (curr != null) {
      if (curr.value % 2 != 0) {
        if (odd == null) {
          odd = curr
          oddTail = curr
        }
        else {
          oddTail.next = curr
          oddTail = oddTail.next
        }
      }
      else {
        if (even == null) {
          even = curr
          evenTail = curr
        }
        else {
          evenTail.next = curr
          evenTail = evenTail.next
        }
      }
      curr = curr.next
    }
    // if the list contains at least one even node
    if (even != null)
    {
      temp = even
      evenTail.next = odd
    }
    else{
      temp = odd
    }
    // null to terminate the list; otherwise, it will go into an infinite loop
    if (oddTail != null) {
      oddTail.next = null
    }
    temp
  }





  def addOneNumber(node: Node[Int],num: Int):Node[Int] = {
    if (node == null) return Node(num)
    val temp = reverseLinkedList_2(node)
    var carry: Int = num
    var curr = temp
    while (carry > 0) {
      val sum = curr.value + carry
      curr.value = sum % 10
      carry = sum / 10
      breakable {
        if (curr.next == null) break
      }
      curr = curr.next
      if (carry > 0) {
        curr.next = Node(carry);
      }
    }
    reverseLinkedList_2(temp)
  }


  def addTwoNumbers(l1: Node[Int], l2: Node[Int]): Node[Int] = {
    def addTwoNumbersHelper(l1: Node[Int], l2: Node[Int], carry: Int): Node[Int] = {
      if (l1 == null && l2 == null && carry == 0) null
      else if (l1 == null && l2 == null && carry != 0) Node[Int](carry)
      else if (l1 == null) addTwoNumbersHelper(Node[Int](0), l2, carry)
      else if (l2 == null) addTwoNumbersHelper(l1, Node[Int](0), carry)
      else Node[Int]((l1.value + l2.value + carry) % 10, addTwoNumbersHelper(l1.next, l2.next, (l1.value + l2.value + carry) / 10))
    }
    addTwoNumbersHelper(l1, l2, 0)
  }

  def reverseListNode(_node: Node[Int], _k: Int): Node[Int] = {
    var reversed: Node[Int] = null
    var node: Node[Int] = _node
    var k: Int = _k
    while (k > 0) {
      val tmp = node.next
      node.next = reversed
      reversed = node
      node = tmp
      k = k - 1
    }
    reversed
  }

  def reverseKGroup(head: Node[Int], k: Int): Node[Int] = {
    var oldHead: Node[Int] = head
    var newHead: Node[Int] = null

    var kthTail: Node[Int] = null
    var node: Node[Int] = oldHead

    while (node != null) {
      var count = 0
      while (count < k && node != null) {
        node = node.next
        count = count + 1
      }
      if (count == k) {
        val reversedNode = reverseListNode(oldHead, k)
        if (newHead == null) newHead = reversedNode
        if (kthTail != null) kthTail.next = reversedNode
        kthTail = oldHead
        oldHead = node
      }
    }
    if (kthTail != null) kthTail.next = oldHead
    if (newHead != null) newHead else head
  }




 // calculate length of loop
  def countNode(node:Node[T]): Int ={
    val temp = node
    @tailrec
    def countRec(node:Node[T], acc:Int=0):Int ={
      if(node.next == temp) acc
      else countRec(node.next, acc+1)
    }
    countRec(node)
  }
  def detectFor(node:Node[T]): Int={
    if (node == null || node.next == null) 0
    else {
      var slow = head
      var fast = head
      while (fast != null && fast.next != null) {
        slow = slow.next
        fast = fast.next.next
        if (slow == fast) return countNode(slow)
      }
      0
    }
  }

  def lenghtOfLoop(node:Node[T]): Int ={
    val res = detectFor(node)
    if(res != 0) res
    else 0
  }
}

object List extends App {
  private val list_1 = new SLinkedList[Int]
  println(list_1.isEmpty)
  list_1.insertAtBeginning(10)
  list_1.insertAtEnd(8)
  list_1.insertAfter(list_1.head, 9)
  list_1.printList()
  println(list_1.list_size())
  list_1.insertAtParticularPosition(7, 0)
  list_1.printList()
  //list_1.remove(7)
  //list_1.printList()
  //println(list_1.list_size())
  //list_1.removeAt(4)
  //list_1.printList()
  //println(list_1.list_size())
  //list_1.removeAtBeginning()
  //list_1.printList()
  //list_1.removeAtEnd()
  //list_1.printList()
  //list_1.search(80)
  //list_1.insertAtEnd(7)
  //list_1.insertAtBeginning(10)
  //list_1.printList()
  //list_1.removeAllOccurrences(9,list_1.head)
  list_1.midElement(list_1.head)
  list_1.midEle(list_1.head)
  list_1.deleteMid(list_1.head)
  list_1.printList()
  list_1.insertAtEnd(0)
  list_1.printList()
  list_1.midElement(list_1.head)
  list_1.midEle(list_1.head)
  //list_1.deleteMid(list_1.head)
  //list_1.printList()


  private val list2 = new SLinkedList[Int]
  private val first = Node(1)
  private val second = Node(2)
  private val third = Node(3)
  private val fourth = Node(4)
  private val five = Node(5)
  list2.head = first
  first.next = second
  second.next = third
  third.next = fourth
  fourth.next = five
  //list2.printList()
  //list2.reverseLinkedList_2(list2.head)
  //list2.printList()
  //list2.removeAll(1)
  list2.printList()
  five.next = second
  //println(list2.detectFor(list2.head))
  println()
  println()
  //list2.printList()
  //list2.midEle(list2.head)
  //list2.deleteMid(list2.head)
  //list2.printList()
  //println(list2.detectLoop(list2.head))


  val list3 = new SLinkedList[Int]

  private val one = Node(1)
  private val two = Node(2)
  private val three = Node(3)
  list3.head = one
  one.next = two
  two.next = three
  //three.next = one
  //list3.printList()
  //println(list3.hasLoop(list3.head))
  //list3.removeLoop(list3.head)
  list3.printList()
  list3.getNthFront(list3.head,0)
  list3.getNthFront(list3.head,2)

  list3.printList()
  list3.getNthLast(list3.head,1)
  list3.getNthLast(list3.head,2)
  list3.getNthLast(list3.head,3)
  list3.printList()
  list3.removeNthNode(list3.head,1)
  list3.printList()
  println()
  println()



  private val list4 = new SLinkedList[Int]
  private val a = Node(1)
  private val b = Node(2)
  private val c = Node(3)
  private val d = Node(4)
  list4.head = a
  a.next = b
  b.next = c
  c.next = d
  list4.printList()
  list4.reverseLinkedList_2(list4.head)
  list4.printList()


  private val list5 = new SLinkedList[Int]
  private val p = Node(0)
  private val q = Node(2)
  private val r = Node(1)
  private val s = Node(0)
  private val t = Node(2)
  list5.head = p
  p.next = q
  q.next = r
  r.next = s
  s.next = t
  list5.printList()
  println(list5.sortList(list5.head))
  println(list5.sortZeroOneTwo(list5.head))
  println(list5.rearrangeEvenOdd(list5.head))
  println(list5.addOneNumber(list5.head,7))

}

