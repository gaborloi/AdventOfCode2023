package org.practice.advent

import scala.annotation.tailrec
import scala.io.BufferedSource

object Task18 {

  case class Cord(r: Int, c: Int) {
    def +(that: Cord): Cord = Cord(r + that.r, c + that.c)

    def -(that: Cord): Cord = Cord(r - that.r, c - that.c)

    def *(that: Cord): Int = r * that.r + c * that.c

    def valid(maxRowIdx: Int, maxColIdx: Int): Boolean = (r <= maxRowIdx) && (c <= maxColIdx) && (r > -1) && (c > -1)

    def *(const: Int): Cord = Cord(r * const, c * const)
  }

  val DIRTOCORD: Map[Char, Cord] = Map('R' -> Cord(0,1), 'L' -> Cord(0,-1), 'D' -> Cord(1,0), 'U' -> Cord(-1,0))
  val DIRTOCHAR: Map[Char, Char] = Map('R' -> '-', 'L' -> '-', 'D' -> '|', 'U' -> '|')
  val pipeDef: Map[String, Char] = Map(
    "DD" -> '|', "UU" -> '|', "LL" -> '-', "RR" -> '-',
    "DL" -> 'J', "DR" -> 'L', "UL" -> '7', "UR" -> 'F',
    "LU" -> 'L', "LD" -> 'F', "RU" -> 'J', "RD" -> '7'
  )
  case class DigOrder(dir: Char, length: Int, color: String)

  def parseline(line: String): DigOrder = {
    val inputs = line.split(" ")
    DigOrder(inputs(0).head, inputs(1).toInt, inputs(2))
  }

  @tailrec
  final def digTheMap(digOrderIt: Iterator[DigOrder], currentPos:Cord, previousDir: Char, mapToDig: Array[Array[Char]]): Array[Array[Char]] = {
    val digOrder = digOrderIt.next()
    val nextDir = DIRTOCORD(digOrder.dir)
    mapToDig(currentPos.r)(currentPos.c) = pipeDef(s"$previousDir${digOrder.dir}")
    for (i <- 1 until  digOrder.length) {
      val updCord = currentPos + nextDir * i
      mapToDig(updCord.r)(updCord.c) = DIRTOCHAR(digOrder.dir)
    }
    val nextCord = currentPos + (nextDir * digOrder.length)
    if (mapToDig(nextCord.r)(nextCord.c) == '.') mapToDig(nextCord.r)(nextCord.c) = DIRTOCHAR(digOrder.dir)
    if (!digOrderIt.hasNext) return mapToDig
    digTheMap(digOrderIt, nextCord, digOrder.dir, mapToDig)
  }

  def isInside(row: String, index: Int): Boolean = {
    if(row.substring(index + 1).contains('#'))
      (row.substring(0, index).replaceAll("#+", "#").replace(".", "").length % 2) == 1 else false
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
  def calcFills(diggedMap: Array[Array[Char]]): Int = {
    var sum = 0

    for (r <- diggedMap.indices; c <- diggedMap.head.indices) {
      if (diggedMap(r)(c) == '.') {
        val res = diggedMap(r).take(c).mkString.replace(".", "").replace("-", "").replace("L7", "|").replace("FJ", "|").length % 2
        if (res == 1) {
//          println(diggedMap(r).take(c).mkString.replace(".", "").replace("-", "").replace("L7", "|").replace("FJ", "|"))
          sum += 1
        }
      } else sum += 1
    }
    sum
  }
  def calcFile1(file: BufferedSource): Int = {
    val digOrders = file.getLines().map(parseline).toList

    val (_, minCord,maxCord) = digOrders.foldLeft((Cord(0,0),Cord(0,0),Cord(0,0))) { case ((curr, min, max), digO) =>
      val next = curr + DIRTOCORD(digO.dir) * digO.length
      (next, Cord(math.min(next.r, min.r), math.min(next.c, min.c)),
        Cord(math.max(next.r, max.r), math.max(next.c, max.c)))
    }
    val length = maxCord - minCord
    val undiggedMap = (for (_ <- 0 to length.r) yield (for (_ <- 0 to length.c) yield '.').toArray).toArray
    val start = Cord(0,0) - minCord
    val diggedBorders = digTheMap(digOrders.iterator, start, digOrders.last.dir, undiggedMap)

    for (arr <- diggedBorders) println(arr.toList)
    calcFills(diggedBorders)
  }

  def calcFile2(file: BufferedSource): Int = {
    2
  }
}