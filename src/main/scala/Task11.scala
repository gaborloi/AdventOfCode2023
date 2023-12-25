package org.practice.advent

import scala.io.BufferedSource

object Task11 {
  case class Galaxy(r: Long, c: Long) {
    def expandRow(idx: Long, expansion: Long): Galaxy = if(r > idx) Galaxy(r + 1L * expansion, c) else this
    def expandCol(idx: Long, expansion: Long): Galaxy = if(c > idx) Galaxy(r, c + 1L * expansion) else this

    def dist(g2: Galaxy): Long = math.abs(g2.r - r) + math.abs(g2.c - c)
  }


  def expandUniverse(universe: Array[Array[Char]], galaxies: Set[Galaxy], expansion: Long): Set[Galaxy] = {
    val emptyRows = universe.zipWithIndex.filter { case (arr,_) => !arr.contains('#') }.map { _._2 }
    val emptyCols = universe.transpose.zipWithIndex.filter { case (arr,_) => !arr.contains('#') }.map { _._2 }

    val rowExpanded =  emptyRows.zipWithIndex.foldLeft(galaxies) { case (gs, (r, i)) =>
      gs.map { g => g.expandRow(r + i * expansion, expansion) }
    }
    emptyCols.zipWithIndex.foldLeft(rowExpanded) { case (gs, (c, i)) =>
      gs.map {g => g.expandCol(c + i * expansion, expansion) }
    }
  }

  def collectGalaxies(universe: Array[Array[Char]]): Set[Galaxy] = (
    for { r <- universe.indices; c <- universe.head.indices if universe(r)(c) == '#' } yield  Galaxy(r.toLong,c.toLong)
  ).toSet
  def calcFile1(file: BufferedSource, expansion: Long): Long = {
    val lines = file.getLines().toList
    val universe = (for {
      line <- lines
    } yield line.toCharArray).toArray
    val galaxies = collectGalaxies(universe)
    val expanded = expandUniverse(universe, galaxies, expansion).toList
    expanded.combinations(2).foldLeft(0L) {
      case (sd, Seq(g1, g2)) =>
        sd + g1.dist(g2)
    }
  }

  def calcFile2(file: BufferedSource, expansion: Long): Long = {
    val lines = file.getLines().toList
    val universe = (for {
      line <- lines
    } yield line.toCharArray).toArray
    val galaxies = collectGalaxies(universe)
    val expanded = expandUniverse(universe, galaxies, expansion).toList
    expanded.combinations(2).foldLeft(0L) {
      case (sd, Seq(g1, g2)) =>
        sd + g1.dist(g2)
    }
  }
}