package dataStructure.graph

import dataStructure.graph.Graph.run

import scala.collection.mutable


// A graph is a collection of nodes where each node might point to other nodes.
// These nodes are connected to each other through a set of edges.
// Graphs are similar to trees, but trees follow certain rules when it comes to their edges:
// 1. A tree with N nodes will have (N-1) edges, one edge for each parent-child relationship.
//    All nodes must be reachable from the root and there must be exactly one possible path from the root to a node.
// 2. In graphs, there are no rules dictating the connection among the nodes. A graph contains a set of nodes and a set of edges,
//    but these edges can connect nodes in any possible way. In fact, trees are actually a subset of graphs.

// Difference - At first, it is important to know that Tree is a special case of Graph where it has no cycles and self loops.
//              So when we say Tree it is basically a form of graph. So above question is ambiguous in that sense.
// 1. 	Each node can have any number of edges vs If there is n nodes then there would be n-1 number of edges
// 2.   They can be directed or undirected vs They are always directed
// 3.   There is no unique node called root in graph vs There is a unique node called root(parent) node in trees.
// 4.   A cycle can be formed vs	There will not be any cycle.
//
// Now why we need graphs at all?
// 1. It makes certain data/information representation easier. If we consider a case say map having cities connected to each other by paths,
//    that’s in itself is our notion of graphs. More examples are flow networks, matching problem. These problems are inherently graphical
// 2. And of course thanks to the blessing of graph data structure, large number of algorithms designed and implemented efficiently.
//    We got problems like flow networks, matching problem, shortest paths, airline scheduling solved.

// Directed Graphs - Directed edges in which connections are one-way. A graph in which all the edges are directed is called a Directed Graphs.
// UnDirected Graphs - Undirected edges in which connections are two-way.Graphs in which all edges are undirected are called Undirected Graphs
// Connected Graphs - In Connected Graph, we can reach to any node from any node
// Complete Graphs - In complete Graph, each node have degree of (n-1), each node must be connected with all node
// Cycle Graph - In Cycle Graph, each node have degree 2 and form a cycle

// The following two are the most commonly used representations of a graph
// 1. Adjacency Matrix is a 2D array of size V x V where V is the number of vertices in a graph. Let the 2D array be adj[][],
//    a slot adj[i][j] = 1 indicates that there is an edge from vertex i to vertex j.
// 2. An array of lists is used. The size of the array is equal to the number of vertices. Let the array be an array[].
//    An entry array[i] represents the list of vertices adjacent to the ith vertex.



// BFS - The breadth-first search (BFS) algorithm is used to search a tree or graph data structure for a node that meets a set of criteria.
// It starts at the tree’s root or graph and searches/visits all nodes at the current depth level before moving on to the nodes at the next depth level. Breadth-first search can be used to solve many problems in graph theory
// Application of BFS
// 1. Social Networking Websites: In social networks, we can find people within a given distance ‘k’ from a person using Breadth First Search till ‘k’ levels.
// 2. GPS Navigation systems: Breadth First Search is used to find all neighboring locations.
// 3. In undirected graphs, either Breadth First Search or Depth First Search can be used to detect cycle. We can use BFS to detect cycle in a directed graph also


// DFS - Depth-first search is an algorithm for traversing or searching tree or graph data structures. The algorithm starts at the root node
// (selecting some arbitrary node as the root node in the case of a graph) and explores as far as possible along each branch before backtracking.
// Applications -  Detecting cycle in a graph, Path Finding



object Graph {

  def adjacencyMatrix(v: Int): Array[Array[Int]] = {
    val matrix = Array.ofDim[Int](v + 1, v + 1)
    for (i <- 0 until v) {
      for (j <- 0 until v) {
        matrix(i)(j) = scala.io.StdIn.readInt()
      }
    }
    matrix
  }

  // we can also use addEdge method to add edges
  def addEdge(matrix: Array[Array[Int]], src: Int, dest: Int): Unit = {
    matrix(src)(dest) = 1
    matrix(dest)(src) = 1
  }

  def adjacencyList(v: Int): Unit = {
    var list: List[List[Int]] = List()
    // Iterate through vertices
    for (i <- 0 until v) {
      val input = scala.io.StdIn.readLine()
      // Split input string and convert to list of integers
      val neighbors = input.split(" ").map(_.toInt).toList
      // Assign list of neighbors to current vertex
      list = list :+ neighbors
    }
  }

  private def bfsTraversal(graph: Map[Int, List[Int]], start: Int): Unit = {
    val visited = Array.fill(graph.size+1)(false)
    val queue = mutable.Queue[Int]()
    visited(start) = true
    queue.enqueue(start)
    while (queue.nonEmpty) {
      val node = queue.dequeue()
      print(node + " ")
      for (neighbor <- graph(node) if !visited(neighbor)) {
        visited(neighbor) = true
        queue.enqueue(neighbor)
      }
    }
    for (i <- graph.keys if !visited(i)) {
      bfsTraversal(graph, i)
    }
  }

  private def dfsTraversal(graph: Map[Int, List[Int]]): Unit = {
    val visited = Array.fill(graph.size + 1)(false)
    def dfsHelper(graph: Map[Int, List[Int]], node: Int, visited: Array[Boolean]): Unit = {
      visited(node) = true
      print(node + " ")
      for (neighbor <- graph(node) if !visited(neighbor)) {
        dfsHelper(graph, neighbor, visited)
      }
      for (i <- graph.keys if !visited(i)) {
        dfsHelper(graph, i, visited)
      }
    }
    dfsHelper(graph,1,visited)
  }

  def dfs(start: Int, graph: Map[Int, List[Int]]): Unit = {
    val visited = Array.fill(graph.keys.max + 1)(false)
    val stack = mutable.Stack[Int](start)
    while (stack.nonEmpty) {
      val vertex = stack.pop()
      if (!visited(vertex)) {
        visited(vertex) = true
        print(vertex + " ")
        graph(vertex).foreach(stack.push)
      }
    }
  }


  def run(): Unit = {
    val graph = Map(0 -> List(1, 2), 1 -> List(2), 2 -> List(0,3), 3 -> List(3))
    bfsTraversal(graph, 2)
    println()
    dfsTraversal(graph)
    println()
    val graphs = Map(1 -> List(2, 3), 2 -> List(4), 3 -> List(4, 5), 4 -> List(), 5 -> List())
    dfs(2, graph)
  }
}
object Graphs extends App{
  run()
}
