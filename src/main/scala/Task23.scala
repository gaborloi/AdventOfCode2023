package org.practice.advent

import org.practice.advent.Task21.GardenMap

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.BufferedSource

object Task23 {

  case class Cord(r: Int, c: Int) {
    def +(that: Cord): Cord = Cord(r + that.r, c + that.c)

    def -(that: Cord): Cord = Cord(r - that.r, c - that.c)

    def *(that: Cord): Int = r * that.r + c * that.c

    def valid(maxRowIdx: Int, maxColIdx: Int): Boolean = (r < maxRowIdx) && (c < maxColIdx) && (r > -1) && (c > -1)

    def *(const: Int): Cord = Cord(r * const, c * const)
  }

  object Cord {
    val STEPS: List[Cord] = List(Cord(1, 0), Cord(0, 1), Cord(-1, 0), Cord(0, -1))
  }

  class PathTracker(val gardenMap: GardenMap) {
    val rowLength: Int = gardenMap.length
    val colLength: Int = gardenMap.head.length
    val trackerMap: Array[Array[Boolean]] =
      (0 until rowLength).map(_ => (0 until colLength).map(_ => false).toArray).toArray
    var currentCord: Cord = Cord(0, gardenMap.head.indexOf('.'))
    val goal: Cord = Cord(rowLength - 1, gardenMap(rowLength - 1).indexOf('.'))
    val path: mutable.Stack[Cord] = mutable.Stack[Cord]()

    def moveTileForward(dir: Cord): Boolean = {
      currentCord += dir
      trackerMap(currentCord.r)(currentCord.c) = true
      path.push(dir)
      slide()
    }

    def fetchTile(cord: Cord): Char = gardenMap(cord.r)(cord.c)
    def checkDirection(dir: Cord): Boolean = {
      val nextCord = dir + currentCord
      nextCord.valid(rowLength, colLength) && (fetchTile(nextCord) != '#') && !checkTracker(nextCord)
    }

    def checkTracker(cord: Cord): Boolean = trackerMap(cord.r)(cord.c)

    def slide(): Boolean = {
      val dir = fetchTile(currentCord) match {
        case '>' => Some(Cord(0, 1))
        case '<' => Some(Cord(0, -1))
        case 'v' => Some(Cord(1, 0))
        case _ => None
      }
      dir match {
        case Some(d) => if (checkDirection(d)) {
          moveTileForward(d)
        } else true
        case None => false
      }
    }

    @tailrec
    final def walkForward(longestPath: Int, availableSteps: List[Cord]): Int = {
      val (longestPathUpd, availableStepsUpd) = availableSteps.find { checkDirection } match {
        case Some(dir) =>
          if (moveTileForward(dir)) {
            val longestPathUpd = if (currentCord == goal) math.max(longestPath, path.length) else longestPath
            if (path.isEmpty) return longestPathUpd
            val lastDir = stepBack()
            (longestPathUpd, Cord.STEPS.dropWhile(_ != lastDir).drop(1))
          } else (longestPath, Cord.STEPS)
        case None =>
          val longestPathUpd = if (currentCord == goal) math.max(longestPath, path.length) else longestPath
          if (path.isEmpty) return longestPathUpd
          val lastDir = stepBack()
          (longestPathUpd, Cord.STEPS.dropWhile(_ != lastDir).drop(1))
      }
      walkForward(longestPathUpd, availableStepsUpd)
    }

    @tailrec
    final def stepBack(): Cord = {
      val dir = path.pop()
      trackerMap(currentCord.r)(currentCord.c) = false
      currentCord -= dir
      if (fetchTile(currentCord) == '.') return dir
      stepBack()
    }
  }

  def calcFile1(file: BufferedSource): Int = {
    val lines = file.getLines().toList
    val tracker = new PathTracker((for {
      line <- lines
    } yield line.toArray).toArray)
    tracker.walkForward(1, Cord.STEPS)
  }

  def calcFile2(file: BufferedSource): Int = {
    val lines = file.getLines().toList
    val tracker = new PathTracker((for {
      line <- lines
    } yield line.replaceAll("<", ".").replaceAll(">", ".").replaceAll("v", ".").toArray).toArray)
    tracker.walkForward(1, Cord.STEPS)
  }
}