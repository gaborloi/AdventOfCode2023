package org.practice.advent

import scala.annotation.tailrec
import scala.io.BufferedSource

object Task10 {
  case class Cord(r: Int, c: Int) {
    def +(that: Cord): Cord = Cord(r + that.r, c + that.c)
    def -(that: Cord): Cord = Cord(r - that.r, c - that.c)
  }

  val cordMap: Map[Char, Cord] =
    Map('l' -> Cord(0,-1), 'r' -> Cord(0,1), 'u' -> Cord(-1,0), 'd' -> Cord(1,0))

  val pipeDef: Map[String, Char] = Map( "du" -> '|', "lr" -> '-', "dl" -> '7', "dr" -> 'F', "lu" -> 'J', "ru" -> 'L')

  case class PipeMap(mapArray: Array[Array[Char]]) {

    val rowMaxIdx: Int = mapArray.length - 1
    val colMaxIdx: Int = mapArray.head.length - 1

    def startingPos(): Cord = {
      for (r <- mapArray.indices; c <- mapArray.head.indices) {
        if (mapArray(r)(c) == 'S') {
          return Cord(r, c)
        }
      }
      Cord(-1,-1)
    }

    def valid(cord: Cord): Boolean = (cord.r >= 0) && (cord.r <= rowMaxIdx) && (cord.c >= 0) && (cord.c <= colMaxIdx)

    val pipeFlowMap: Map[Char, Map[Char, Char]] = Map(
      '|' -> Map('u' -> 'u', 'd' -> 'd'),
      '-' -> Map('l' -> 'l', 'r' -> 'r'),
      'L' -> Map('u' -> 'l', 'r' -> 'd'),
      'J' -> Map('u' -> 'r', 'l' -> 'd'),
      '7' -> Map('l' -> 'u', 'd' -> 'r'),
      'F' -> Map('r' -> 'u', 'd' -> 'l'),
    )

    def findNewCord(cord: Cord, dirFrom: Char, pipe: Char): (Cord, Char) = {
      val newDirFrom = pipeFlowMap.getOrElse(pipe, Map()).getOrElse(dirFrom, 'x')
      val diff = cordMap.getOrElse(newDirFrom, Cord(0,0))
      (cord - diff, newDirFrom)
    }

    @tailrec
    final def iterateThrough(cord: Cord, dirFrom: Char, iteration: Int): Int = {
      val pipe = mapArray(cord.r)(cord.c)
      if (pipe == 'S') return iteration
      val (nextCord, dir) = findNewCord(cord, dirFrom, mapArray(cord.r)(cord.c))
      if (dir == 'x') return 0
      iterateThrough(nextCord, dir, iteration + 1)
    }

    @tailrec
    final def collectLoopList(cord: Cord, dirFrom: Char, loopList: List[Cord]): List[Cord] = {
      val pipe = mapArray(cord.r)(cord.c)
      val updList: List[Cord] = loopList :+ cord
      if (pipe == 'S') return updList
      val (nextCord, dir) = findNewCord(cord, dirFrom, mapArray(cord.r)(cord.c))
      if (dir == 'x') return List()
      collectLoopList(nextCord, dir, updList)
    }

    def findStartChar(loopList: List[Cord]): Char = {
      val reverseCordMap = cordMap.map { case (k,v) => v -> k }
      val st = reverseCordMap(loopList.head - loopList.last).toString +
        reverseCordMap(loopList(loopList.size - 2) - loopList.last).toString
      pipeDef(st.sorted)
    }
    def generateCleanMap(loopList: List[Cord], startPipe: Char): Array[Array[Char]] = {
      val cleanArray = mapArray.clone()
      for (r <- cleanArray.indices; c <- cleanArray.head.indices) {
        val cord = Cord(r, c)
        if (loopList.contains(cord)) {
          if (cleanArray(r)(c) == 'S') {
            cleanArray(r)(c) = startPipe
          }
        } else cleanArray(r)(c) = '.'
      }
      cleanArray
    }

    def checkCords(cleanArray: Array[Array[Char]]): Set[Cord] = {
      var enclosedSet: Set[Cord] = Set()
      for (r <- cleanArray.indices; c <- cleanArray.head.indices) {
        if (cleanArray(r)(c) == '.') {
          val res = cleanArray(r).take(c).mkString.replace(".", "").replace("-", "").replace("L7", "|").replace("FJ", "|").length % 2
          if (res == 1) {
            println(cleanArray(r).take(c).mkString.replace(".", "").replace("-", "").replace("L7", "|").replace("FJ", "|"))
            enclosedSet += Cord(r, c)
          }
        }
      }
      enclosedSet
    }
  }

  def calcFile1(file: BufferedSource): Int = {
    val lines = file.getLines().toList
    val charArray = (for {
      line <- lines
    } yield line.toCharArray).toArray
    val pipeMap = PipeMap(charArray)
    val startCord = pipeMap.startingPos()
    cordMap foreach { case (dirFrom, diff) =>
      val res = pipeMap.iterateThrough(startCord - diff, dirFrom, 0)
      if (res > 0) return (res + 1) / 2
    }
    0
  }

  def calcFile2(file: BufferedSource): Int = {
    val lines = file.getLines().toList
    val charArray = (for {
      line <- lines
    } yield line.toCharArray).toArray

//    for (arr <- charArray) println(arr.toList)
    val pipeMap = PipeMap(charArray)
    val startCord = pipeMap.startingPos()
    val loopList = cordMap.foldLeft(List[Cord]()) { case (lL, (dirFrom, diff)) =>
      val newCord = startCord - diff
      if (pipeMap.valid(newCord)) {
        val iteratedList = pipeMap.collectLoopList(newCord, dirFrom, List())
        if (iteratedList.nonEmpty) iteratedList else lL
      } else lL
    }
    val startPipe = pipeMap.findStartChar(loopList)

    val cleanArray = pipeMap.generateCleanMap(loopList, startPipe)
//    for (arr <- cleanArray) println(arr.toList)
    val enclosedSet = pipeMap.checkCords(cleanArray)

    enclosedSet.size
  }
}