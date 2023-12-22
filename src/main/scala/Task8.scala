package org.practice.advent

import scala.annotation.tailrec
import scala.io.BufferedSource
import scala.collection.immutable.TreeMap

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
  def calcFile2(file: BufferedSource): Int = {
    val lines = file.getLines()
    val instructions = parseInstruction("", lines)
    val locationMap: MyMap = lines.map { line =>
      line.substring(0, 3) -> (line.substring(7, 10), line.substring(12, 15))
    }.toMap
    val pathFinder = GhostPathFinder(instructions, locationMap)
    pathFinder.execute(locationMap.keys.filter(_.endsWith("A")).toSeq, 0)
  }
}