package dataStructure.tree

import dataStructure.tree.BinaryTree.run

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

// A tree data structure is defined as a collection of objects or entities known as nodes
// that are linked together to represent or simulate hierarchy.

// A tree data structure is a non-linear data structure because it does not store in a sequential manner.
// It is a hierarchical structure as elements in a Tree are arranged in multiple levels

// The depth of node x can be defined as the length of the path from the root to the node x
// The height of node x can be defined as the longest path from the node x to the leaf node.

// Types of Tree data structure

// 1. General Tree - In the general tree, a node can have either 0 or maximum n number of nodes. There is no restriction
// 2. Binary Tree -  In a binary tree, each node in a tree can have utmost two child nodes.
//                   Here, utmost means whether the node has 0 nodes, 1 node or 2 nodes.
//
// 3. Binary Search Tree - A node can be connected to the utmost two child nodes in a binary search tree, so the node contains two pointers (left child and right child pointer).
//                         Every node in the left subtree must contain a value less than the value of the root node, and the value of each node in the right subtree must be bigger than the value of the root node
//
// 4. AVL Tree - AVL tree satisfies the property of the binary tree as well as of the binary search tree. It is a self-balancing binary search tree
//               self-balancing means that balancing the heights of left subtree and right subtree. This balancing is measured in terms of the balancing factor.
//               The balancing factor can be defined as the difference between the height of the left subtree and the height of the right subtree
//               The balancing factor's value must be either 0, -1, or 1; therefore, each node in the AVL tree should have the value of the balancing factor either as 0, -1, or 1.
//
// 5. Red Black Tree - The red-Black tree is the binary search tree.
//                     the red-black tree is a self-balancing binary search tree. AVL tree is also a height balancing binary search tree then why do we require a Red-Black tree
//                     In the AVL tree, we do not know how many rotations would be required to balance the tree, but in the Red-black tree, a maximum of 2 rotations are required to balance the tree
//                     It contains one extra bit that represents either the red or black color of a node to ensure the balancing of the tree.
//                     When any operation is performed on the tree, we want our tree to be balanced so that all the operations like searching, insertion, deletion, etc., take less time, and all these operations will have the time complexity of log2n


// 6. Full/ proper/ strict Binary tree - The full binary tree is also known as a strict binary tree. The tree can only be considered as the full binary tree if each node must contain either 0 or 2 children
//                                       The full binary tree can also be defined as the tree in which each node must contain 2 children except the leaf node
//
// 7. Complete Binary Tree -   The complete binary tree is a tree in which all the nodes are completely filled except the last level. In the last level, all the nodes must be as left as possible.
//                             In a complete binary tree, the nodes should be added from the left.
//
// 8. Balanced Binary Tree -   The balanced binary tree is a tree in which both the left and right trees differ by atmost 1. For example, AVL and Red-Black trees are balanced binary tree.

// application of tree - Storing Naturally Hierarchical Data and Organize Data
// application of binary tree - binary trees are mainly used for searching and sorting as they provide a means to store data hierarchically.
// application of BST - BSTs are used for indexing, BSTs can be used to store and quickly retrieve data in computer simulations

// Difference between BST and Binary Tree
// 1. BINARY TREE is a nonlinear data structure where each node can have at most two child nodes
//    where BINARY SEARCH TREE is a node based binary tree that further has right and left subtree that too are binary search tree.
// 2. BINARY TREE is unordered hence slower in process of insertion, deletion, and searching
//    where Insertion, deletion, searching of an element is faster in BINARY SEARCH TREE than BINARY TREE due to the ordered characteristics
// 3. Binary trees allow duplicate values where Binary Search Tree does not allow duplicate values.
// 4. It is used for retrieval of fast and quick information and data lookup where It works well at element deletion, insertion, and searching



object BinaryTree{
  case class Node(var data: Int, var left: Node = null, var right: Node = null)

  def insert(node: Node, data: Int): Node = {
    if (node == null) {
       Node(data)
    } else {
      if (data <= node.data) {
        node.left = insert(node.left, data)
      } else {
        node.right = insert(node.right, data)
      }
      node
    }
  }

  @tailrec
  def search(node: Node, data:Int): Boolean ={
    if(node == null) false
    else if(node.data == data) true
    else{
      if(data < node.data) search(node.left,data)
      else search(node.right,data)
    }
  }

  private def deleteNode(root: Node, key: Int): Node = {
    if (root == null) return root
    if (key < root.data)
      root.left = deleteNode(root.left, key)
    else if (key > root.data)
      root.right = deleteNode(root.right, key)
    else {
      if (root.left == null) return root.right
      else if (root.right == null) return root.left
      var minNode = root.right
      while (minNode.left != null) minNode = minNode.left
      root.data = minNode.data
      root.right = deleteNode(root.right, minNode.data)
    }
    root
  }


  private def buildTreeUsingArr(nodes: Array[Int], i: Int): Node = {
    var root: Node = null
    if (i < nodes.length) {
      root = Node(nodes(i))
      root.left = buildTreeUsingArr(nodes, 2 * i + 1)
      root.right = buildTreeUsingArr(nodes, 2 * i + 2)
    }
    root
  }

  private def preOrder(node: Node): Unit = {
    if (node == null) return
    else {
      print(node.data + " ")
      preOrder(node.left)
      preOrder(node.right)
    }
  }

  def preOrderTraversal(root: Node): Unit = {
    val stack = mutable.Stack[Node]()
    var current = root
    stack.push(current)
    while (stack.nonEmpty) {
      current = stack.pop()
      println(current.data)
      if (current.right != null) stack.push(current.right)
      if (current.left != null) stack.push(current.left)
    }
  }


  private def inOrder(node: Node): Unit = {
    if (node != null) {
      inOrder(node.left)
      print(node.data + " ")
      inOrder(node.right)
    }
  }

  def inorderTraversal(root: Node): Unit = {
    val stack = mutable.Stack[Node]()
    var current = root
    while (current != null || stack.nonEmpty) {
      while (current != null) {
        stack.push(current)
        current = current.left
      }
      current = stack.pop()
      print(current.data + " ")
      current = current.right
    }
  }


  private def postOrder(node: Node): Unit = {
    if (node != null) {
      postOrder(node.left)
      postOrder(node.right)
      print(node.data + " ")
    }
  }

  def postOrderTraversal(root: Node): Unit = {
    val s1 = mutable.Stack[Node]()
    val s2 = mutable.Stack[Node]()
    s1.push(root)
    while (s1.nonEmpty) {
      val current = s1.pop()
      s2.push(current)
      if (current.left != null) s1.push(current.left)
      if (current.right != null) s1.push(current.right)
    }
    while (s2.nonEmpty) {
      println(s2.pop().data)
    }
  }


  private def printCurrentLevel(node: Node, level: Int): Unit = {
    if (node == null) return
    if (level == 0) print(node.data + " ")
    else {
      printCurrentLevel(node.left, level - 1)
      printCurrentLevel(node.right, level - 1)
    }
  }

  def height(node: Node): Int = {
    if (node == null) -1
    else{
      val lHeight = height(node.left)
      val rHeight = height(node.right)
      if (lHeight > rHeight) lHeight + 1
      else rHeight + 1
    }
  }

  private def levelOrderRecursive(node: Node): Unit = {
    val h = height(node)
    for (i <- 0 to h) {
      printCurrentLevel(node, i)
    }
  }

  private def levelOrderTraversal(root: Node): Unit = {
    val q = mutable.Queue[Node]()
    q.enqueue(root)
    while (q.nonEmpty) {
      val current = q.dequeue()
      print(current.data + " ")
      if (current.left != null) q.enqueue(current.left)
      if (current.right != null) q.enqueue(current.right)
    }
  }

  def zigzagTraversal(root: Node): List[Int] = {
    if (root == null) return List()
    var result = List[Int]()
    val queue = scala.collection.mutable.Queue[Node]()
    queue.enqueue(root)
    var leftToRight = true
    while (queue.nonEmpty) {
      var size = queue.size
      var level = List[Int]()
      while (size > 0) {
        val curr = queue.dequeue()
        level = level :+ curr.data
        if (curr.left != null) queue.enqueue(curr.left)
        if (curr.right != null) queue.enqueue(curr.right)
        size -= 1
      }
      if (!leftToRight) level = level.reverse
      leftToRight = !leftToRight
      result = result ++ level
    }
    result
  }


  def boundaryTraversal(root: Node): List[Int] = {
    if (root == null) return List()
    var result = List[Int](root.data)
    result = result ++ leftBoundary(root.left)
    result = result ++ leaves(root)
    result = result ++ rightBoundary(root.right)
    result
  }

  def leftBoundary(node: Node): List[Int] = {
    if (node == null) return List()
    if (node.left == null && node.right == null) return List()
    var result = List[Int](node.data)
    if (node.left != null) result = result ++ leftBoundary(node.left)
    else result = result ++ leftBoundary(node.right)
    result
  }

  def rightBoundary(node: Node): List[Int] = {
    if (node == null) return List()
    if (node.left == null && node.right == null) return List()
    var result = List[Int]()
    if (node.right != null) result = rightBoundary(node.right) ++ result
    else result = rightBoundary(node.left) ++ result
    result = result :+ node.data
    result
  }

  def leaves(node: Node): List[Int] = {
    if (node == null) return List()
    if (node.left == null && node.right == null) return List(node.data)
    val result = leaves(node.left) ++ leaves(node.right)
    result
  }


  private def countNodesRec(node: Node): Int = {
    if(node == null) 0
    else 1 + countNodesRec(node.left) + countNodesRec(node.right)
  }

  private def countNodes(root: Node): Int = {
    var count = 0
    val queue = mutable.Queue[Node]()
    queue.enqueue(root)
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      count += 1
      if (current.left != null) queue.enqueue(current.left)
      if (current.right != null) queue.enqueue(current.right)
    }
    count
  }

   private def sumNodesRec(node: Node): Int = {
     if (node == null) 0
     else node.data + sumNodesRec(node.left) + sumNodesRec(node.right)
  }

  private def sumNodes(root: Node): Int = {
    var sum = 0
    val queue = mutable.Queue[Node]()
    queue.enqueue(root)
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      sum += current.data
      if (current.left != null) queue.enqueue(current.left)
      if (current.right != null) queue.enqueue(current.right)
    }
    sum
  }

  private def printLeafNodesRec(node: Node): Unit = {
    if (node.left == null && node.right == null) print(node.data + " ")
    if (node.left != null) printLeafNodesRec(node.left)
    if (node.right != null) printLeafNodesRec(node.right)
  }

  private def printLeafNodes(root: Node): Unit = {
    val queue = mutable.Queue[Node]()
    queue.enqueue(root)
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (current.left == null && current.right == null) {
        print(current.data + " ")
      }
      if (current.right != null) queue.enqueue(current.right)
      if (current.left != null) queue.enqueue(current.left)

    }
  }

  private def sumLeafNodesRec(node: Node): Int = {
    var sum = 0
    if (node.left == null && node.right == null) sum += node.data
    if (node.left != null) sum += sumLeafNodesRec(node.left)
    if (node.right != null) sum += sumLeafNodesRec(node.right)
    sum
  }

  private def sumLeafNodes(root: Node): Int = {
    var sum = 0
    val queue = mutable.Queue[Node]()
    queue.enqueue(root)
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (current.left == null && current.right == null) {
        sum += current.data
      }
      if (current.right != null) queue.enqueue(current.right)
      if (current.left != null) queue.enqueue(current.left)
    }
    sum
  }


  private def sumLongestPathRec(node: Node): Int = {
    if (node == null) {
      return 0
    }
    val leftSum = sumLongestPathRec(node.left)
    val rightSum = sumLongestPathRec(node.right)
     math.max(leftSum, rightSum) + node.data
  }

  //  it is not straightforward to determine which path is the longest while traversing the tree in a breadth-first manner.
  private def maxPathSum(root: Node): Int = {
    val queue = scala.collection.mutable.Queue[Node]()
    var maxSum = Int.MinValue
    queue.enqueue(root)
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (current.left == null && current.right == null) {
        maxSum = Math.max(maxSum, current.data)
      }
      if (current.left != null) {
        current.left.data += current.data
        queue.enqueue(current.left)
      }
      if (current.right != null) {
        current.right.data += current.data
        queue.enqueue(current.right)
      }
    }
    maxSum
  }

  private var maxDiameter = 0
  private def diameterOfBinaryTree(root: Node): Int = {
    maxDiameter = 0
    maxDepth(root)
    maxDiameter
  }
  private def maxDepth(node: Node): Int = {
    if (node == null) return 0
    val left = maxDepth(node.left)
    val right = maxDepth(node.right)
    maxDiameter = Math.max(maxDiameter, left + right)
    Math.max(left, right) + 1
  }

  //  giving error, priority of the code is 2
//  def treeToDoublyList(root: Node): Node = {
//    var prev: Node = null
//    var head: Node = null
//    inOrder(root, prev, head)
//    head
//  }
//  private def inOrder(node: Node, prev: Node, head: Node): Unit = {
//    if (node == null) return
//    inOrder(node.left, prev, head)
//    if (prev == null) head = node
//     else {
//      prev.right = node
//      node.left = prev
//    }
//    prev = node
//    inOrder(node.right, prev, head)
//  }


  def isBST(node: Node, minValue: Int, maxValue: Int): Boolean = {
    if (node == null) return true
    if (node.data < minValue || node.data > maxValue) return false
    isBST(node.left, minValue, node.data - 1) && isBST(node.right, node.data + 1, maxValue)
  }
  def checkBST(node: Node): Boolean = {
    isBST(node, Int.MinValue, Int.MaxValue)
  }

  var prev: Node = null
  def isValidBST(root: Node): Boolean = {
    if (root != null) {
      if (!isValidBST(root.left)) false
      if (prev != null && root.data <= prev.data) false
      prev = root
      isValidBST(root.right)
    }
    true
  }

// 1 2 4 5
  def isHeightBalanced(tree: Node): Boolean = {
    def checkHeight(tree: Node): Int = {
      if (tree == null) return 0
      val leftHeight = checkHeight(tree.left)
      if (leftHeight == -1) return -1
      val rightHeight = checkHeight(tree.right)
      if (rightHeight == -1) return -1
      val heightDiff = leftHeight - rightHeight
      if (Math.abs(heightDiff) > 1) return -1
      Math.max(leftHeight, rightHeight) + 1
    }
    checkHeight(tree) != -1
  }

  private def isSameTree(p: Node, q: Node): Boolean = {
    if (p == null && q == null) true
    else if (p == null || q == null) false
    else if (p.data != q.data) false
    else isSameTree(p.left, q.left) && isSameTree(p.right, q.right)
  }


  def isSymmetric(root: Node): Boolean = {
    recursion(root, root)
  }
  private def recursion(root1: Node, root2: Node): Boolean = {
    if (root1 == null && root2 == null) true
    else if (root1 == null || root2 == null) false
    else if (root1.data != root2.data) false
    else {
      val left = recursion(root1.left, root2.right)
      val right = recursion(root1.right, root2.left)
      if (!left) false
      else if (!right) false
      else true
    }
  }

  private def topView(root: Node): Unit = {
    if (root == null) return
    val map = scala.collection.mutable.Map[Int, Int]()
    val queue = scala.collection.mutable.Queue[(Node, Int)]()
    queue.enqueue((root, 0))
    while (queue.nonEmpty) {
      val (node, hd) = queue.dequeue()
      if (!map.contains(hd)) {
        map(hd) = node.data
      }
      if (node.left != null) queue.enqueue((node.left, hd - 1))
      if (node.right != null) queue.enqueue((node.right, hd + 1))
    }
    for (hd <- map.keys.toList.sorted) {
      print(map(hd) + " ")
    }
  }

  private def bottomView(root: Node): Unit = {
    if(root == null) return
    val map = scala.collection.mutable.Map[Int, Int]()
    val queue = scala.collection.mutable.Queue[(Node, Int)]()
    queue.enqueue((root, 0))
    while (queue.nonEmpty) {
      val (node, hd) = queue.dequeue()
      map(hd) = node.data
      if (node.left != null) queue.enqueue((node.left, hd - 1))
      if (node.right != null) queue.enqueue((node.right, hd + 1))
    }
    for (hd <- map.keys.toList.sorted) {
      print(map(hd) + " ")
    }
  }

  private def leftView(root: Node): Unit = {
    if (root == null) return
    val queue = new mutable.Queue[Node]
    queue.enqueue(root)
    while (queue.nonEmpty) {
      val size = queue.size
      var i = 0
      while (i < size) {
        val current = queue.dequeue()
        if (i == 0) print(current.data + " ")
        if (current.left != null) queue.enqueue(current.left)
        if (current.right != null) queue.enqueue(current.right)
        i += 1
      }
    }
  }

  private def rightView(root: Node): Unit = {
    if (root == null) return
    val queue = new mutable.Queue[Node]
    queue.enqueue(root)
    while (queue.nonEmpty) {
      val size = queue.size
      var i = 0
      while (i < size) {
        val current = queue.dequeue()
        if (i == size-1) print(current.data + " ")
        if (current.left != null) queue.enqueue(current.left)
        if (current.right != null) queue.enqueue(current.right)
        i += 1
      }
    }
  }

  private def maximum(root: Node): Int ={
    if(root == null)  Integer.MIN_VALUE
    else Math.max(root.data,Math.max(maximum(root.left),maximum(root.right)))
  }

  def minimum(root: Node): Int ={
    if(root == null) Integer.MAX_VALUE
    else Math.min(root.data,Math.min(minimum(root.left),minimum(root.right)))
  }

  private def lca(root: Node, n1: Int, n2: Int): Node ={
    if(root == null) null
    else if(root.data == n1 || root.data == n2 ) root
    else{
      val left = lca(root.left,n1,n2)
      val right = lca(root.right,n1,n2)
      if(left == null) right
      else if(right == null) left
      else root
    }
  }

  def maxWidth(root: Node): Int = {
    if (root == null) return 0
    var maxWidth = 0
    val queue = new scala.collection.mutable.Queue[Node]()
    queue.enqueue(root)
    while (queue.nonEmpty) {
      maxWidth = math.max(maxWidth, queue.size)
      for (i <- 0 until queue.size) {
        val node = queue.dequeue()
        if (node.left != null) queue.enqueue(node.left)
        if (node.right != null) queue.enqueue(node.right)
      }
    }
    maxWidth
  }

  private def printPaths(root: Node, path: List[Int]): Unit = {
    if (root == null) return
    val newPath = path :+ root.data
    if (root.left == null && root.right == null) {
      println(newPath)
    } else {
      printPaths(root.left, newPath)
      printPaths(root.right, newPath)
    }
  }
  private def printAllPaths(root: Node): Unit = {
    printPaths(root, List[Int]())
  }


  private def findPath(root: Node, target: Node, path: List[Int]): Boolean = {
    if (root == null) return false
    val newPath = path :+ root.data
    if (root == target) {
      println(newPath)
      return true
    }
    if (findPath(root.left, target, newPath) || findPath(root.right, target, newPath)) {
      return true
    }
    false
  }
  private def printPath(root: Node, target: Node): Unit = {
    val path = findPath(root, target, List[Int]())
    if(!path) println("No Path")
  }


  def run(): Unit ={
    var root: Node = Node(1)
    root = insert(root, 2)
    root = insert(root, 3)
    root = insert(root, 4)
    root = insert(root, 5)
    root = insert(root, 6)
    root = insert(root, 7)
    println(search(root, 121))
    val nodes = Array(1, 2, 3, 4, 5, 6, 7)
    val root2 = buildTreeUsingArr(nodes,0)
    println(root2)

//    inOrder(root)
//    println()
//    preOrder(root)
//    println()
//    postOrder(root)
//    println()
//    println(height(root))
//    levelOrderRecursive(root)
//    println()
//    println()
//    println(deleteNode(root,5))
//    inOrder(root)
//    println()
//    postOrder(root)
//    println()
//    println(countNodes(root))
//    println(sumNodes(root))
//    println(heightOfTree(root))
//    printLeafNodes(root)

  println()
  println()

    val tree = Node(1)
  tree.left = Node(2)
  tree.right = Node(3)
  tree.left.left = Node(4)
  tree.left.right = Node(5)
  tree.right.left = Node(6)
  tree.right.right = Node(7)

  inOrder(tree)
  println()
  preOrder(tree)
  println()
  postOrder(tree)
    println()
    levelOrderRecursive(tree)
    println()
//    println(height(tree))
//    levelOrderTraversal(tree)
//    println()
//    println(countNodes(tree))
//    println(sumNodes(tree))
//    printLeafNodes(tree)
//    println()
//    println(sumLeafNodes(tree))
//    println(sumLongestPathRec(tree))
//    println(maxPathSum(tree))
//    println(diameterOfBinaryTree(tree))
    println(maximum(tree))
    println(minimum(tree))
    println()
    println()

    inOrder(tree)
    println()
    println(zigzagTraversal(tree))
    println(boundaryTraversal(tree))
    topView(tree)
    println()
    bottomView(tree)
    println()
    leftView(tree)
    println()
    rightView(tree)
    println()
    println(diameterOfBinaryTree(tree))
    println(lca(tree,2,3))
    printAllPaths(tree)
    printPath(tree, tree.left.left)
  }
}




object Main extends App{
  run()
}


