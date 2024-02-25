package org.practice.advent

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.BufferedSource

object Task25 {

  class Edge(vertexA: String, vertexB: String) {
    val vertex1: String = if (vertexA < vertexB) vertexA else vertexB
    val vertex2: String = if (vertexA < vertexB) vertexB else vertexA

    override def equals(obj: Any): Boolean = obj match {
      case e: Edge => (this.vertex1 == e.vertex1) && (this.vertex2 == e.vertex2)
    }

    override def toString: String = s"$vertex1-$vertex2"
  }
  class Graph(val edges: List[Edge]) {
    val vertices: Set[String] = edges.foldLeft(Set[String]()) { (s, e) => s.union(Set(e.vertex1, e.vertex2)) }

    def findEdges(v: String): List[Edge] = edges.filter { e => e.vertex1 == v || e.vertex2 == v }

    def findNeighbours(v: String): Set[String] =
      findEdges(v).map { e => if(e.vertex1 == v) e.vertex2 else e.vertex1 }.toSet

    def dropEdges(toDrop: List[Edge]): Graph = new Graph(edges.filterNot( e => toDrop.contains(e)))
  }


  class FindIndependentPath(val start: String, val graph: Graph) {

    val accessedVertices: mutable.Map[String, List[List[Edge]]] = mutable.Map()
    private val edgeToCover: mutable.Queue[(String, List[Edge])] = mutable.Queue().addOne((start, List()))

    def findPaths(pathMinLength: Int, minCount: Int): List[Edge] = {
      addEdge()
      println(accessedVertices.map { case (k, v) => k -> v.size } )
      val sp = accessedVertices.values.flatten
      val filterValue = sp.map(_.size).max - pathMinLength
      println(sp.map(_.size).max)
      val filtered = sp.filter(_.size >= filterValue)
      val subset = filtered.foldLeft(Set[Edge]()) { (s, l) => s.union(l.toSet) }
      val counts = subset.map { e => e -> filtered.count(_.contains(e)) }.toMap.filter(_._2 > minCount).keys.toList
      counts
    }

    @tailrec
    final def addEdge(): Unit = {
      val (nextVertex, path) = edgeToCover.dequeue()

      val (extendedPaths, hasNew) = accessedVertices.get(nextVertex) match {
        case Some(savedPaths) => if (savedPaths.forall { savedPath => savedPath.forall(!path.contains(_)) }) {
          (savedPaths :+ path, true)
        } else (savedPaths, false)
        case None => (List(path), true)
      }

      if(hasNew) {
        accessedVertices(nextVertex) = extendedPaths
        graph.findEdges(nextVertex).foreach { e =>
          if (path.isEmpty || e != path.last) {
            val v = if (nextVertex == e.vertex1) e.vertex2 else e.vertex1
            if (v != start) edgeToCover.addOne((v, path :+ e))
          }
        }
      }
      if(edgeToCover.isEmpty) return
      addEdge()
    }
  }

  def parseLine(l: String): List[Edge] = {
    val sp = l.split(":")
    val firstV = sp.head
    val otherVs = sp(1).drop(1).split(" ").toList
    otherVs.map( v => new Edge(firstV, v))
  }

  @tailrec
  def walkthroughGraph(g: Graph, neighbourVertices: Set[String], coveredVertices: Set[String]): Int = {
    val newVertices = neighbourVertices.foldLeft(Set[String]()) { (newNeighbours, v) =>
      newNeighbours.union(g.findNeighbours(v).diff(coveredVertices))
    }
    if (newVertices.isEmpty) return coveredVertices.size

    walkthroughGraph(g, newVertices, coveredVertices.union(newVertices))
  }

  def calcFile1(file: BufferedSource, pathLengthCount: Int, minPathLength: Int): Int = {
    val edges = file.getLines().flatMap(parseLine).toList
    val g = new Graph(edges)
    val start = g.vertices.head
    val algo = new FindIndependentPath(start, g)

    val edgesToTest = algo.findPaths(pathLengthCount, minPathLength)

    val vertexCount = g.vertices.size
    val init = Set(start)
    edgesToTest.combinations(3) foreach { dropEdges =>

      val groupCount = walkthroughGraph(g.dropEdges(dropEdges), init, init )
      if (groupCount < vertexCount) return groupCount * ( vertexCount - groupCount)
    }
    0
  }
}