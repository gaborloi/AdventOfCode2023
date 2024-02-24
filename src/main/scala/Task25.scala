package org.practice.advent

import org.practice.advent.Task25.Edge

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
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
  class Graph(it: Iterator[String]) {
    val edges: List[Edge] = it.flatMap { l => parseLine(l) }.toList

    val vertices: Set[String] = edges.foldLeft(Set[String]()) { (s, e) => s.union(Set(e.vertex1, e.vertex2)) }

    private def parseLine(l: String): List[Edge] = {
      val sp = l.split(":")
      val firstV = sp.head
      val otherVs = sp(1).drop(1).split(" ").toList
      otherVs.map( v => new Edge(firstV, v))
    }

    def findEdges(v: String): List[Edge] = edges.filter { e => e.vertex1 == v || e.vertex2 == v }
  }

  class PathEvaluator() {
    var shortestPaths: ListBuffer[HashSet[Edge]] = ListBuffer[HashSet[Edge]]()
    var independentPathCount = 0

    def evalPath(path: HashSet[Edge]): Boolean = {
      if (shortestPaths.forall { sp => sp.intersect(path).isEmpty }) {
        addIndependentPath(path)
        return true
      }
      if (!shortestPaths.forall { sp => sp.diff(path).nonEmpty }) return false //sp < path
      val keepPaths = shortestPaths.filter { sp => path.diff(sp).nonEmpty } // sp > path
      if (keepPaths.size != shortestPaths.size) {
        shortestPaths = keepPaths :+ path
        independentPathCount = recalculatePathCount()
      }
      true
    }

    def recalculatePathCount(): Int = {
      if (shortestPaths.size < 2) shortestPaths.size else {
        for (elem <- (2 to shortestPaths.size)) {
          val hasIndependent = shortestPaths.combinations(elem).exists { lb =>
            lb.reduce { (a, b) => a.union(b) }.size == lb.map(_.size).sum
          }
          if (!hasIndependent) return elem - 1
        }
      shortestPaths.size
      }
    }
    def addIndependentPath(path: HashSet[Edge]): Unit = {
      independentPathCount += 1
      shortestPaths += path
    }
  }

  class FindIndependentPath(val start: String, val graph: Graph) {

    val accessedVertices: mutable.Map[String, PathEvaluator] = mutable.Map[String, PathEvaluator]()

    def findPaths(): Int = {
      addEdge(HashSet[Edge](), start)
      val indepentPathsCount = accessedVertices.map { case (v, pe) => v -> pe.independentPathCount }.toMap
      println(indepentPathsCount)
      val relevantVertices = accessedVertices.filter { case (_, pe) => pe.independentPathCount == 3 }

//      println(relevantVertices.map { case (v, paths) => v -> paths.map (_.size) })
//      println(relevantVertices("rzs"))
      indepentPathsCount.min._2
    }

    def addEdge(path: HashSet[Edge], nextVertex: String): Unit = {
      println(nextVertex, path)
      accessedVertices.get(nextVertex) match {
        case Some(savedPaths) => if (!savedPaths.evalPath(path)) return
        case None => 1
      }

      graph.findEdges(nextVertex).foreach { e =>
        if(path.isEmpty || e != path.last) {
          val v = if (nextVertex == e.vertex1) e.vertex2 else e.vertex1
          if (v != start) addEdge(path + e , v)
        }
      }
    }
  }

  def calcFile1(file: BufferedSource): Int = {
    val g = new Graph(file.getLines())
    val algo = new FindIndependentPath(g.vertices.head, g)
    algo.findPaths()
  }

  def calcFile2(file: BufferedSource): Int = {
    2
  }
}