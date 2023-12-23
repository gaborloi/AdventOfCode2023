package org.practice.advent

import scala.annotation.tailrec
import scala.io.BufferedSource

object Task8 {

  type MyMap = Map[String, (String, String)]

  case class PathFinder(instructions: String, locationMap: MyMap) {
    val length: Int = instructions.length

    @tailrec
    final def execute(location: String, instructionIdx: Int): Int = {
      val nextLocation = instructions(instructionIdx % length) match {
        case 'L' => locationMap(location)._1
        case 'R' => locationMap(location)._2
      }
      if (nextLocation == "ZZZ") return instructionIdx + 1
      execute(nextLocation, instructionIdx + 1)
    }
  }

  case class GhostPathFinder(instructions: String, locationMap: MyMap) {
    val length: Int = instructions.length

    @tailrec
    final def execute(locations: Seq[String], instructionIdx: Int): Int = {
      val nextLocations = instructions(instructionIdx % length) match {
        case 'L' => locations.map(locationMap(_)._1)
        case 'R' => locations.map(locationMap(_)._2)
      }
      if (nextLocations.forall(_.endsWith("Z"))) return instructionIdx + 1
      execute(nextLocations, instructionIdx + 1)
    }
    @tailrec
    final def findRecursion(location: String, instructionIdx: Int, visited: Seq[String]): Seq[String] = {
      val directionIndex = instructionIdx % length
      val upd: Seq[String] = if (visited.contains(location + directionIndex)) {
        return visited.appended(location + directionIndex.toString)
      } else visited.appended(location + directionIndex.toString)

      val nextLocation = instructions(directionIndex) match {
        case 'L' => locationMap(location)._1
        case 'R' => locationMap(location)._2
      }
      findRecursion(nextLocation, instructionIdx + 1, upd)
    }
  }

  def findCycle(locations: Seq[String]): Int = {
    val cycleStart = locations.zipWithIndex.findLast {
      case (l,i) => (l == locations.last) && (i < locations.size - 1)
    }.get
    locations.size - cycleStart._2
  }

  def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(1: BigInt){
    (a, b) => b * a /
      Stream.iterate( (a, b) ) { case (x,y) => (y: BigInt, x%y) }.dropWhile(_._2 != 0).head._1.abs
  }

  @tailrec
  def parseInstruction(instruction: String, lineIterator: Iterator[String]): String = {
    lineIterator.next() match {
      case "" => instruction
      case l => parseInstruction(instruction + l, lineIterator)
    }
  }

  def calcFile1(file: BufferedSource): Int = {

    val lines = file.getLines()
    val instructions = parseInstruction("", lines)
    val locationMap: MyMap = lines.map { line =>
      line.substring(0, 3) -> (line.substring(7, 10), line.substring(12, 15))
    }.toMap
    val pathFinder = PathFinder(instructions, locationMap)
    pathFinder.execute("AAA", 0)
  }

  def calcFile2(file: BufferedSource): BigInt = {
    val lines = file.getLines()
    val instructions = parseInstruction("", lines)
    val locationMap: MyMap = lines.map { line =>
      line.substring(0, 3) -> (line.substring(7, 10), line.substring(12, 15))
    }.toMap
    val pathFinder = GhostPathFinder(instructions, locationMap)

    val sizes = locationMap.keys.filter(_.endsWith("A")).toSeq.map { loc =>
      val path = pathFinder.findRecursion(loc, 0, Seq())
      val cycle = findCycle(path)
      println(cycle)
      println(path.last)
      path.zipWithIndex.filter { case (l, _) => l(2) == 'Z' }
    }
    println(sizes)
    lcm(sizes.map(_.head._2))
  }
}