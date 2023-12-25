package org.practice.advent

import scala.annotation.tailrec
import scala.io.BufferedSource

object Task11 {
  case class Galaxy(r: Int, c: Int, expansion: Int) {
    def expandRow(idx: Int): Galaxy = if(r > idx) Galaxy(r + 1, c) else this
    def expandCol(idx: Int): Galaxy = if(c > idx) Galaxy(r, c + 1) else this


    def dist(g2: Galaxy): Int = math.abs(g2.r - r) + math.abs(g2.c - c)
  }


  def expandUniverse(universe: Array[Array[Char]], galaxies: Set[Galaxy]): Set[Galaxy] = {
    val emptyRows = universe.zipWithIndex.filter { case (arr,_) => !arr.contains('#') }.map { _._2 }
    val emptyCols = universe.transpose.zipWithIndex.filter { case (arr,_) => !arr.contains('#') }.map { _._2 }

    val rowExpanded =  emptyRows.zipWithIndex.foldLeft(galaxies) { case (gs, (r, i)) =>
      gs.map { g => g.expandRow(r + i) }
    }
    emptyCols.zipWithIndex.foldLeft(rowExpanded) { case (gs, (c, i)) => gs.map {g => g.expandCol(c + i) } }
  }

  def collectGalaxies(universe: Array[Array[Char]]): Set[Galaxy] = (
    for { r <- universe.indices; c <- universe.head.indices if universe(r)(c) == '#' } yield  Galaxy(r,c)
  ).toSet
  def calcFile1(file: BufferedSource): Int = {
    val lines = file.getLines().toList
    val universe = (for {
      line <- lines
    } yield line.toCharArray).toArray
    val galaxies = collectGalaxies(universe)
    val expanded = expandUniverse(universe, galaxies).toList
    expanded.combinations(2).foldLeft(0) {
      case (sd, Seq(g1, g2)) =>
        sd + g1.dist(g2)
    }
  }

  def calcFile2(file: BufferedSource): Int = {
    val lines = file.getLines().toList
    val universe = (for {
      line <- lines
    } yield line.toCharArray).toArray
    val galaxies = collectGalaxies(universe)
    val expanded = expandUniverse(universe, galaxies).toList
    expanded.combinations(2).foldLeft(0) {
      case (sd, Seq(g1, g2)) =>
        sd + g1.dist(g2)
    }
  }
}