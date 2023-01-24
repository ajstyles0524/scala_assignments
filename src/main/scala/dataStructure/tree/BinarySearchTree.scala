package dataStructure.tree

import dataStructure.tree.BinarySearchTree.run

import scala.annotation.tailrec

object BinarySearchTree {
  case class Node(var data: Int, var left: Node = null, var right: Node = null)

  def insertRec(node: Node, data: Int): Node = {
    if (node == null) Node(data)
    else {
      if (data < node.data) node.left = insertRec(node.left, data)
      else node.right = insertRec(node.right, data)
      node
    }
  }

  private def insert(node: Node, data: Int): Node ={
    val newNode: Node = Node(data)
    var curr : Node = node
    var parent: Node = null

    while(curr != null){
      parent = curr
      if(data < curr.data) curr = curr.left
      else curr = curr.right
    }
    if(parent == null) parent = newNode
    else if(data < parent.data) parent.left = newNode
    else parent.right = newNode
    parent
  }

  private def inOrder(node: Node): Unit = {
    if (node != null) {
      inOrder(node.left)
      print(node.data + " ")
      inOrder(node.right)
    }
  }

  @tailrec
  def searchBST(root: Node, value: Int): Boolean = {
    if (root == null) false
    else if(root.data == value) true
    else if (root.data > value)  searchBST(root.left, value)
    else  searchBST(root.right, value)
  }

  def minimum(root: Node): Int = {
    if(root == null) Integer.MAX_VALUE
    else Math.min(root.data,Math.min(minimum(root.left),minimum(root.right)))
  }

  def maximum(root: Node): Int = {
    if (root == null) Integer.MIN_VALUE
    else Math.max(root.data, Math.max(maximum(root.left), maximum(root.right)))
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

  def isBST(node: Node, minValue: Int, maxValue: Int): Boolean = {
    if (node == null) return true
    if (node.data < minValue || node.data > maxValue) return false
    isBST(node.left, minValue, node.data - 1) && isBST(node.right, node.data + 1, maxValue)
  }
  private def checkBST(node: Node): Boolean = {
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


  private def floor(node:Node, value: Int): Int ={
    var root = node
    var ans = Integer.MAX_VALUE
    while(root != null){
      if(root.data == value) return root.data
      else if(root.data > value) root = root.left
      else{
        ans = root.data
        root = root.right
      }
    }
    ans
  }

  private def ceil(node: Node, value: Int): Int ={
    var root = node
    var ans = Integer.MIN_VALUE
    while (root != null) {
      if (root.data == value) return root.data
      else if (root.data > value) {
        ans = root.data
        root = root.left
      }
      else {
        root = root.right
      }
    }
    ans
  }

  def findTarget(root: Node, k: Int): Boolean = {
    val set = scala.collection.mutable.Set[Int]()
    def dfs(node: Node, k: Int, set: scala.collection.mutable.Set[Int]): Boolean = {
      if (node == null) return false
      if (dfs(node.left, k, set)) return true
      if (set.contains(k - node.data)) return true
      set += node.data
      dfs(node.right, k, set)
    }
    dfs(root, k, set)
  }




  def run(): Unit ={
    var root = Node(1)
    root = insertRec(root, 2)
    root = insertRec(root, 3)
    root = insertRec(root, 4)
    root = insertRec(root, 5)
    root = insertRec(root, 6)
    root = insertRec(root, 7)
    inOrder(root)
    println()
    println(searchBST(root,1))
    println(minimum(root))
    println(maximum(root))
    println(isValidBST(root))
    println(checkBST(root))
    println(floor(root,10))
    println(ceil(root,5))
  }
}

object BST extends App{
  run()
}
