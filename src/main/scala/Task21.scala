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

    def evalCord(c: Cord, newValue: Char): Set[Cord] = {
      Cord.STEPS.foldLeft(Set[Cord]()) { (s, stp) =>
        val cord = c + stp
        if (!cord.valid(rowMax, colMax)) s
        else {
          val value = garden(cord.r)(cord.c)
          value match {
            case c if newValue == c || c == '#' => s
            case '.' =>
              garden(cord.r)(cord.c) = newValue
              s + cord
            case 'S' =>
              garden(cord.r)(cord.c) = newValue
              s
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
    final def step(maxStep:Int, currentStep: Int, lastNewPos: Set[Cord]): Int = {
      val currentTurn = (currentStep % 2).toString.head

      val newPos = lastNewPos.flatMap(pos => evalCord(pos, currentTurn))
      if (maxStep == currentStep + 1) return garden.map(arr => arr.count( p => p == currentTurn)).sum
      step(maxStep,currentStep + 1,  newPos)
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
      if (maxStep == currentStep + 1) return newCount
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

  def calcFile1(file: BufferedSource, maxStep: Int): Int = {
    val lines = file.getLines().toList
    val garden: Garden = Garden((for {
      line <- lines
    } yield line.toArray).toArray)
//    garden.garden foreach (arr => println(arr.mkString))
    val hashtagcount = garden.garden.foldLeft(0) { (s,arr) => s + arr.count(_ == '#') }
    println(hashtagcount)
    println(garden.rowMax, garden.colMax, garden.startCord)
    println((131*131-hashtagcount)/2)
    garden.step(maxStep, 0, Set(garden.startCord))
    garden.garden foreach (arr => println(arr.mkString))
    1
  }

  def calcFile2(file: BufferedSource, maxStep: Int): Long = {
    val lines = file.getLines().toList
    val garden: Garden = Garden((for {
      line <- lines
    } yield line.toArray).toArray)
    //    garden.garden foreach (arr => println(arr.mkString))
    garden.stepInfinite(maxStep, 0, Set(garden.startCord), 1, Set(), 0)
  }
}