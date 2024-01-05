package org.practice.advent

import scala.annotation.tailrec
import scala.io.BufferedSource

object Task18 {
  implicit class RangeImprovements(r: Range) {
    def myIntersect(that: Range): Range = Range.inclusive(math.max(r.min, that.min), math.min(r.max, that.max))

    def splitRange(splittingRange: Range): List[Range] = {
      val intersect = r myIntersect splittingRange
      if (intersect.isEmpty) List() else {
        List(
          Range(r.min, intersect.min),
          intersect,
          Range.inclusive(intersect.max + 1, r.max)
        )
      }
    }
  }

  case class Cord(r: Int, c: Int) {
    def +(that: Cord): Cord = Cord(r + that.r, c + that.c)

    def -(that: Cord): Cord = Cord(r - that.r, c - that.c)

    def *(that: Cord): Int = r * that.r + c * that.c

    def *(const: Int): Cord = Cord(r * const, c * const)
  }

  case class SparseMapRow(rows: Range, sparsePipe: List[(Char, Int)])

  val DIRTOCORD: Map[Char, Cord] = Map('R' -> Cord(0,1), 'L' -> Cord(0,-1), 'D' -> Cord(1,0), 'U' -> Cord(-1,0))
  val DIRTOCHAR: Map[Char, Char] = Map('R' -> '-', 'L' -> '-', 'D' -> '|', 'U' -> '|')
  val pipeDef: Map[String, Char] = Map(
    "DD" -> '|', "UU" -> '|', "LL" -> '-', "RR" -> '-',
    "DL" -> 'J', "DR" -> 'L', "UL" -> 'T', "UR" -> 'F',
    "LU" -> 'L', "LD" -> 'F', "RU" -> 'J', "RD" -> 'T'
  )
  case class DigOrder(dir: Char, length: Int)

  def parseline(line: String): DigOrder = {
    val inputs = line.split(" ")
    DigOrder(inputs(0).head, inputs(1).toInt)
  }

  val hexlastToDir: Map[Char, Char] = Map('0' -> 'R', '1' -> 'D', '2' -> 'L', '3' -> 'U')

  def parseline2(line: String): DigOrder = {
    val inputs = line.split(" ")(2)
    DigOrder(hexlastToDir(inputs.charAt(7)), Integer.parseInt(inputs.substring(2,7), 16))
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

  def updateSparseMapRow(
    currentPos: Cord, pipeType: Char, pipeConnectionType: Char, originalSmr: SparseMapRow, updatedRanges: List[Range]
  ): List[SparseMapRow] = {
    val minSmr = if (updatedRanges.head.nonEmpty)
      List(SparseMapRow(updatedRanges.head, originalSmr.sparsePipe))
    else List()
    val maxSmr = if (updatedRanges(2).nonEmpty)
      List(SparseMapRow(updatedRanges(2), originalSmr.sparsePipe))
    else List()

    val updPipeInfo = pipeType match {
      case '|' =>  originalSmr.sparsePipe :+ (pipeType, currentPos.c)
      case '-' => originalSmr.sparsePipe
    }

    val midSplit = updatedRanges(1).splitRange(Range.inclusive(currentPos.r, currentPos.r)) match {
      case withStartingPoint if withStartingPoint.size == 3 =>
        List(
          SparseMapRow(withStartingPoint.head, updPipeInfo),
          SparseMapRow(withStartingPoint(1), originalSmr.sparsePipe :+ (pipeConnectionType, currentPos.c)),
          SparseMapRow(withStartingPoint(2), updPipeInfo),
        )
      case noStartingPoint if noStartingPoint.isEmpty => List(SparseMapRow(updatedRanges(1), updPipeInfo))
    }
    (minSmr ++ midSplit ++ maxSmr).filter { smr => smr.rows.nonEmpty}
  }

  @tailrec
  final def digTheMapSparse(
    digOrderIt: Iterator[DigOrder],
    currentPos: Cord,
    previousDir: Char,
    sparseMap: List[SparseMapRow],
  ): List[SparseMapRow] = {
    val digOrder = digOrderIt.next()
    val nextDir = DIRTOCORD(digOrder.dir)
    val orderEndRow = currentPos.r + nextDir.r * (digOrder.length - 1)
    val orderRange = Range.inclusive(math.min(currentPos.r, orderEndRow), math.max(currentPos.r, orderEndRow))
    val updSpaseMap = sparseMap.flatMap { smr =>
      val ranges = smr.rows.splitRange(orderRange)
      ranges match {
        case noSplit if noSplit.isEmpty => List(smr)
        case intersected if intersected.size == 3 =>
          updateSparseMapRow(
            currentPos, DIRTOCHAR(digOrder.dir), pipeDef(s"$previousDir${digOrder.dir}"), smr, intersected
          )
      }
    }

    val nextCord = currentPos + (nextDir * digOrder.length)
    if (!digOrderIt.hasNext) return updSpaseMap
    digTheMapSparse(digOrderIt, nextCord, digOrder.dir, updSpaseMap)
  }

  def checkPipeString(pipe: String): Boolean =
    pipe.replace(".", "").replace("-", "").replace("LT", "|").replace("FJ", "|").length % 2 == 1

  def calcFills(diggedMap: Array[Array[Char]]): Int = {
    var sum = 0

    for (r <- diggedMap.indices; c <- diggedMap.head.indices) {
      if (diggedMap(r)(c) == '.') {
        val res = checkPipeString(diggedMap(r).take(c).mkString)
        if (res) sum += 1
      } else sum += 1
    }
    sum
  }

  @tailrec
  final def evalRow(pipeIt: Iterator[(Char, Int)], pipeString: String, previousIdx: Int, sum: Long): Long = {
    val (c, idx) = pipeIt.next()
    val updSum = c match {
      case vertical if vertical == 'L' || vertical == 'F' || vertical == '|' =>
        if (checkPipeString(pipeString)) sum + (idx - previousIdx - 1).toLong else sum
      case vertical if vertical == 'T' || vertical == 'J' => sum
    }
//    println(updSum)
    if (!pipeIt.hasNext) updSum else evalRow(pipeIt, pipeString :+ c, idx, updSum)
  }

  def calcFillsSparse(diggedMap: List[SparseMapRow]): Long = diggedMap.foldLeft(0L) { (sum, smr) =>
    val ordered = smr.sparsePipe.sortWith { case ((_,idx1), (_, idx2)) => idx1 < idx2  }
    println(sum, ordered)
//    println(evalRow(ordered.iterator, "", 0, 0L))
    sum + evalRow(ordered.iterator, "", 0, 0L) * smr.rows.size.toLong
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
    1
  }

  def calcFile2(file: BufferedSource): Long = {
    val digOrders = file.getLines().map(parseline2).toList
    val (_, minCord,maxCord) = digOrders.foldLeft((Cord(0,0),Cord(0,0),Cord(0,0))) { case ((curr, min, max), digO) =>
      val next = curr + DIRTOCORD(digO.dir) * digO.length
      (next, Cord(math.min(next.r, min.r), math.min(next.c, min.c)),
        Cord(math.max(next.r, max.r), math.max(next.c, max.c)))
    }
    println(minCord, maxCord)
    val maxCordMod = maxCord - minCord
    val minCordMod = Cord(0, 0)
    val start = Cord(0,0) - minCord
    val sparseMapInit = List(SparseMapRow(Range.inclusive(minCordMod.r, maxCordMod.r),List()))
    val diggedBorders = digTheMapSparse(digOrders.iterator, start, digOrders.last.dir, sparseMapInit)
    diggedBorders foreach { db =>
      println(db.rows, db.sparsePipe)
    }
    calcFillsSparse(diggedBorders) + digOrders.map(_.length.toLong).sum
  }
}