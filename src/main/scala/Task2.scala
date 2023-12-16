package org.practice.advent

import scala.io.BufferedSource

object Task2 {

  //Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
  private val colorLimits = Map("red" -> 12, "green" -> 13, "blue" -> 14)
  def parseLine(line: String): Int = {
    var hasGameId = false
    var gameId = ""
    var value = ""
    var color = ""
    for (c <- line) {
      if (hasGameId) {
        c match {
          case d if d.isDigit => value += d
          case l if l.isLetter => color += l
          case ',' | ';' =>
            if (colorLimits(color) < value.toInt) return 0 else {
              value = ""
              color = ""
            }
          case _ => ""
        }
      } else c match {
        case d if d.isDigit => gameId += c
        case ':' => hasGameId = true
        case _ => ""
      }
    }
    if (colorLimits(color) < value.toInt) return 0
    println(s"$gameId: $line")
    gameId.toInt
  }

  def parseLineTask2(line: String): Int = {
    var hasGameId = false
    var gameId = ""
    var value = ""
    var color = ""
    val colorValues = collection.mutable.Map("red" -> 0, "green" -> 0, "blue" -> 0)
    for (c <- line) {
      if (hasGameId) {
        c match {
          case d if d.isDigit => value += d
          case l if l.isLetter => color += l
          case ',' | ';' =>
            colorValues(color) = Math.max(colorValues(color), value.toInt)
            value = ""
            color = ""
          case _ => ""
        }
      } else c match {
        case d if d.isDigit => gameId += c
        case ':' => hasGameId = true
        case _ => ""
      }
    }
    colorValues(color) = Math.max(colorValues(color), value.toInt)
    println(s"$colorValues: $line")
    colorValues("blue") * colorValues("green") * colorValues("red")
  }
  def parseFile(file: BufferedSource): Int = file.getLines().foldLeft(0) {
    (n, s) => parseLine(s) + n
  }

  def parseFile2(file: BufferedSource): Int = file.getLines().foldLeft(0) {
    (n, s) => parseLineTask2(s) + n
  }
}
