package org.practice.advent

import scala.annotation.tailrec
import scala.io.BufferedSource

object Task21 {

  type GardenMap = Array[Array[Char]]
  case class Garden(garden: GardenMap) {
    val startCord: Cord = findStart()
    val rowMax: Int = garden.length
    val colMax: Int = garden.head.length

    def findStart(): Cord = {
      for (r <- garden.indices; c <- garden.head.indices) {
        if(garden(r)(c) == 'S') return Cord(r,c)
      }
      Cord(-1, -1)
    }

    def shiftGarden(dir: Cord): GardenMap = {
      val newGarden = garden.map(_.clone())
      for ( r <- garden.indices; c <- garden.head.indices) {
        val cc = Cord(r, c) - dir
        val gRow = if (cc.r % rowMax < 0) rowMax + (cc.r % rowMax) else cc.r % rowMax
        val gCol = if (cc.c % colMax < 0) colMax + (cc.c % colMax) else cc.c % colMax
        newGarden(r)(c) = garden(gRow)(gCol)
      }
      newGarden
    }

    def evalCord(c: Cord): Set[Cord] = {
      Cord.STEPS.foldLeft(Set[Cord]()) { (s, stp) =>
        val cord = c + stp
        if (!cord.valid(rowMax, colMax)) s
        else {
          val value = garden(cord.r)(cord.c)
          value match {
            case '#' => s
            case '.' | 'S' => s + cord
            case _ => s
          }
        }
      }
    }

    def evalCordInf(c: Cord): Set[Cord] = {
      Cord.STEPS.foldLeft(Set[Cord]()) { (s, stp) =>
        val cord = c + stp
        val gRow = if (cord.r % rowMax < 0) rowMax + (cord.r % rowMax) else cord.r % rowMax
        val gCol = if (cord.c % colMax < 0) colMax + (cord.c % colMax) else cord.c % colMax
        garden(gRow)(gCol) match {
          case '#' => s
          case '.' | 'S' => s + cord
        }
      }
    }

    @tailrec
    final def step(
      maxStep:Int,
      currentStep: Int,
      prePos: Set[Cord],
      preCount: Int,
      preprePos: Set[Cord],
      prepreCount: Int): (Int, Int) = {
      val newPos = prePos.flatMap(pos => evalCord(pos)).diff(preprePos)
      val newCount = prepreCount + newPos.size
      if ((maxStep == currentStep + 1) || newPos.isEmpty) {
        return if((maxStep - 1) % 2 == currentStep % 2) {
          (newCount, preCount)
        } else (preCount, newCount)
      }
      step(maxStep, currentStep + 1,  newPos, newCount, prePos, preCount)
    }

    @tailrec
    final def diffFromOptimal(
      maxStep:Int,
      currentStep: Int,
      prePos: Set[Cord],
      preprePos: Set[Cord],
      newGardenMap: Array[Array[Int]]
    ): Array[Array[Int]] = {
      val newPos = prePos.flatMap(pos => evalCordInf(pos)).diff(preprePos)
      newPos.foreach { c =>
        if (c.valid(rowMax, colMax)) {
          newGardenMap(c.r)(c.c) =
            currentStep + 1
        }
      }
      if (maxStep == currentStep + 1) return newGardenMap
      diffFromOptimal(maxStep, currentStep + 1,  newPos, prePos, newGardenMap)
    }

    @tailrec
    final def stepInfinite(
      maxStep:Int,
      currentStep: Int,
      prePos: Set[Cord],
      preCount: Long,
      preprePos: Set[Cord],
      prepreCount: Long
    ): Long = {
      val newPos = prePos.flatMap(pos => evalCordInf(pos)).diff(preprePos)
      val newCount = prepreCount + newPos.size.toLong
      if ((maxStep == currentStep + 1) || newPos.isEmpty) {
        return if ((maxStep - 1) % 2 == currentStep % 2) {
          newCount
        } else preCount
      }
      stepInfinite(maxStep, currentStep + 1,  newPos, newCount, prePos, preCount)
    }
  }
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

  def calcFullyCoveredGardens(garden: Garden, maxStep: Int): Long = {

    val (evenCount, oddCount) = garden.step(10000, 0, Set(garden.startCord), 1, Set(), 0)

    (1 until maxStep / garden.rowMax).foldLeft(0L) { (sum, idx) =>
      val count = if((idx + maxStep) % 2 == 0) oddCount.toLong else evenCount.toLong
      sum + idx.toLong * count
    }
  }

  def calcNStep(step: Int, diffFromOpt: Array[Array[Int]]): Long =
    diffFromOpt.zipWithIndex.foldLeft(0L) { case (count, (arr, r)) =>
      count + arr.zipWithIndex.count { case (n, c) => ((r + c) % 2) == (step % 2) && (n <= step) && (n >= 0) }
  }
  def calcQuarterPlane(garden: Garden, maxStep: Int): Long = {
    val fcg = calcFullyCoveredGardens(garden, maxStep)
    val diffFromOpt = garden.diffFromOptimal(300, 0, Set(garden.startCord), Set(), garden.garden.map(_.map(_ => -1)))
    diffFromOpt(garden.startCord.r)(garden.startCord.c) = 0
    val fullRound = calcNStep(garden.rowMax + maxStep % garden.rowMax, diffFromOpt) * (maxStep / garden.rowMax)
    val resid = calcNStep(maxStep % garden.rowMax, diffFromOpt) * ((maxStep / garden.rowMax) + 1)
    fcg + resid + fullRound
  }

  def calcFile1(file: BufferedSource, maxStep: Int): Int = {
    val lines = file.getLines().toList
    val garden: Garden = Garden((for {
      line <- lines
    } yield line.toArray).toArray)
    garden.step(maxStep, 0, Set(garden.startCord), 1, Set(), 0)._1
  }

  def calcFile2(file: BufferedSource, maxStep: Int): Long = {
    val lines = file.getLines().toList
    val garden: Garden = Garden((for {
      line <- lines
    } yield line.toArray).toArray)

    val rdG = Cord(0,0)
    val luG = Cord(garden.rowMax - 1, garden.colMax - 1)
    val ldG = Cord(0, garden.colMax - 1)
    val ruG = Cord(garden.rowMax - 1, 0)

    List(rdG, luG, ldG, ruG).foldLeft(0L) { (sum, gm) =>
      val g = Garden(garden.shiftGarden(gm - garden.startCord))
      sum + calcQuarterPlane(g, maxStep)
    } - (maxStep / 2 + 1) * 4 + 1 * (maxStep - 1) % 2
  }

  def calcFile3(file: BufferedSource, maxStep: Int): Long = {
    val lines = file.getLines().toList
    val garden: Garden = Garden((for {
      line <- lines
    } yield line.toArray).toArray)
    garden.stepInfinite(maxStep, 0, Set(garden.startCord), 1, Set(), 0)
  }
}