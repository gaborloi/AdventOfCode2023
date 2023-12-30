package org.practice.advent

import scala.annotation.tailrec
import scala.io.BufferedSource

object Task16 {

  type Contraption = Array[Array[Char]]
  case class Cord(r: Int, c: Int) {
    def +(that: Cord): Cord = Cord(r + that.r, c + that.c)
    def -(that: Cord): Cord = Cord(r - that.r, c - that.c)

    def rightMirror(): Cord = Cord(-c, -r) // (0,1) -> (-1,0), (-1,0) -> (0, 1), (1,0) => (0, -1)

    def leftMirror(): Cord = Cord(c, r) // \ (0,1) -> (1,0), (-1,0) -> (0, -1), (1,0) => (0, 1)
  }

  case class Beam(cord: Cord, dir: Cord) {
    def rightMirrored(): Beam = {
      val newDir = dir.rightMirror()
      Beam(cord + newDir, newDir)
    }
    def leftMirrored(): Beam = {
      val newDir = dir.leftMirror()
      Beam(cord + newDir, newDir)
    }

    def valid(mapSize: Int): Boolean = (cord.r < mapSize) && (cord.c < mapSize) && (cord.r > -1) && (cord.c > -1)

    def forward(): Beam = Beam(cord + dir, dir)
    def splitted(): List[Beam] = List(leftMirrored(), rightMirrored())
    def transform(contraption: Contraption): List[Beam] = {
      contraption(cord.r)(cord.c) match {
        case '.' => List(forward())
        case '/' => List(rightMirrored())
        case '\\' => List(leftMirrored())
        case '-' => if (dir.r != 0) splitted() else List(forward())
        case '|' => if (dir.c != 0) splitted() else List(forward())
      }
    }
  }
  @tailrec
  def moveBeams(currentBeams: Set[Beam], beamHistory: Set[Beam], contraption: Contraption): Set[Beam] = {
    val nextBeamSet = currentBeams.foldLeft(List[Beam]()) { (nl, b) =>
      val nextBeams: List[Beam] = b.transform(contraption).filter { bb => bb.valid(contraption.length) }
      nl ++ nextBeams
    }.toSet
    val uniqueBeams = nextBeamSet.diff(beamHistory)
    if (uniqueBeams.isEmpty) return beamHistory
    moveBeams(uniqueBeams, nextBeamSet.concat(beamHistory), contraption)
  }

  def checkDir(initBeams: List[Beam], contraption: Contraption): Int = initBeams.foldLeft(0) { (n, b) =>
    math.max(n, moveBeams(Set(b), Set(b), contraption).map(_.cord).size)
  }
  def calcFile1(file: BufferedSource): Int = {
    val lines = file.getLines().toList
    val contraption: Contraption = (for {
      line <- lines
    } yield line.toCharArray).toArray
    val energized = moveBeams(Set(Beam(Cord(0,0), Cord(0,1))), Set(Beam(Cord(0,0), Cord(0,1))), contraption)
    energized.map(_.cord).size
  }

  def calcFile2(file: BufferedSource): Int = {
    val lines = file.getLines().toList
    val contraption: Contraption = (for {
      line <- lines
    } yield line.toCharArray).toArray
    val inits = contraption.indices.flatMap(i => List(
      Beam(Cord(i,0), Cord(0,1)),
      Beam(Cord(i, contraption.length - 1), Cord(0,-1)),
      Beam(Cord(0,i), Cord(1,0)),
      Beam(Cord(contraption.length - 1, i), Cord(-1,0))
      )
    )

    checkDir(inits.toList, contraption)
  }
}