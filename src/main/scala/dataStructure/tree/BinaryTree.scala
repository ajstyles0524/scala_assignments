package dataStructure.tree
import dataStructure.tree.BinaryTree.{Node, inOrder, insert, run}

import javax.swing.tree.TreeNode

object BinaryTree{
  class Node(var data: Int, var left: Node, var right: Node)

  def inOrder(node: Node) {
    if (node != null) {
      inOrder(node.left)
      print(node.data + " ")
      inOrder(node.right)
    }
  }

  def preOrder(node: Node): Unit = {
    if (node != null) {
      print(node.data + " ")
      preOrder(node.left)
      preOrder(node.right)
    }
  }

  def postOrder(node: Node) {
    if (node != null) {
      postOrder(node.left)
      postOrder(node.right)
      print(node.data + " ")
    }
  }

  def printCurrentLevel(node: Node, level: Int) {
    if (node == null) return
    if (level == 1) println(node.data)
    else {
      printCurrentLevel(node.left, level - 1)
      printCurrentLevel(node.right, level - 1)
    }
  }

  def height(node: Node): Int = {
    if (node == null) return 0
    val lHeight = height(node.left)
    val rHeight = height(node.right)
    if (lHeight > rHeight)  lHeight + 1
    else  rHeight + 1
  }

  def levelOrderRecursive(node: Node) {
    val h = height(node)
    for (i <- 1 to h) {
      printCurrentLevel(node, i)
    }
  }

  def countNodes(node: Node): Int = {
    var count = 1
    if (node.left != null) {
      count += countNodes(node.left)
    }
    if (node.right != null) {
      count += countNodes(node.right)
    }
    count
  }

  def sumNodes(node: Node): Int = {
    var sum = node.data
    if (node.left != null) {
      sum += sumNodes(node.left)
    }
    if (node.right != null) {
      sum += sumNodes(node.right)
    }
    sum
  }

  def heightOfTree(node: Node): Int = {
    if (node == null) {
      return 0
    }
    val leftHeight = height(node.left)
    val rightHeight = height(node.right)
    math.max(leftHeight, rightHeight) + 1
  }

  def printLeafNodes(node: Node): Unit = {
    if (node.left == null && node.right == null) {
      print(node.data)
    }
    if (node.left != null) {
      printLeafNodes(node.left)
    }
    if (node.right != null) {
      printLeafNodes(node.right)
    }
  }

  def sumLeafNodes(node: Node): Int = {
    if (node.left == null && node.right == null) {
      return node.data
    }
    var sum = 0
    if (node.left != null) {
      sum += sumLeafNodes(node.left)
    }
    if (node.right != null) {
      sum += sumLeafNodes(node.right)
    }
    sum
  }

  def sumLongestPath(node: Node): Int = {
    if (node == null) {
      return 0
    }
    val leftSum = sumLongestPath(node.left)
    val rightSum = sumLongestPath(node.right)
     math.max(leftSum, rightSum) + node.data
  }


  def isBST(node: Node, minValue: Int, maxValue: Int): Boolean = {
    if (node == null) {
      return true
    }
    if (node.data < minValue || node.data > maxValue) {
      return false
    }
    isBST(node.left, minValue, node.data - 1) && isBST(node.right, node.data + 1, maxValue)
  }

  def checkBST(node: Node): Boolean = {
    isBST(node, Int.MinValue, Int.MaxValue)
  }


  def insert(node: Node, data: Int): Node = {
    if (node == null) {
      new Node(data, null, null)
    } else {
      if (data <= node.data) {
        node.left = insert(node.left, data)
      } else {
        node.right = insert(node.right, data)
      }
      node
    }
  }

  def isHeightBalanced(tree: Node): Boolean = {
    def checkHeight(tree: Node): Int = {
      if (tree == null) return 0
      val leftHeight = checkHeight(tree.left)
      if (leftHeight == -1) return -1
      val rightHeight = checkHeight(tree.right)
      if (rightHeight == -1) return -1
      val heightDiff = leftHeight - rightHeight
      if (Math.abs(heightDiff) > 1) return -1
      return Math.max(leftHeight, rightHeight) + 1
    }

    checkHeight(tree) != -1
  }


  def run(): Unit ={
    var root: Node = null
    root = insert(root, 5)
    root = insert(root, 3)
    root = insert(root, 2)
    root = insert(root, 4)
    root = insert(root, 7)
    root = insert(root, 6)
    root = insert(root, 8)
    inOrder(root)
    println()
    preOrder(root)
    println()
    postOrder(root)
    println()
    println(countNodes(root))
    println(sumNodes(root))
    println(heightOfTree(root))
    printLeafNodes(root)
  }
}




object Main extends App{
  run()
}


