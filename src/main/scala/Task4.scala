package org.practice.advent

import scala.io.BufferedSource
import scala.util.matching.Regex

object Task4 {
private val gamePattern: Regex = "Card\\s*\\d*:\\s*(.*) \\|\\s*(.*)".r
  def parseLine(input: String): Int = {
    gamePattern.findFirstMatchIn(input) match {
      case Some(m) =>
        val winnings = m.group(1)
        val own = m.group(2)
        val intersect = winnings.split(" +").toSet.intersect(own.split(" +").toSet)
        println(input, intersect)
      if (intersect.nonEmpty) math.pow(2, intersect.size - 1).toInt else 0
    }
  }

  def parseLine2(idx: Int, maxIdx: Int, input: String, wonCardArray: Array[Int]): Array[Int] = {
    gamePattern.findFirstMatchIn(input) match  {
      case Some(m) =>
        val winnings = m.group(1)
        val own = m.group(2)
        val intersect = winnings.split(" +").toSet.intersect(own.split(" +").toSet)
        wonCardArray(idx) += 1
        for (i <- idx + 1  to math.min(idx + intersect.size, maxIdx)) {
          wonCardArray(i) += wonCardArray(idx)
        }
        wonCardArray
    }
  }

  def parseFile1(file: BufferedSource): Int = file.getLines().foldLeft(0) {
    (n,s) => parseLine(s) + n
  }

  def parseFile2(file: BufferedSource): Int = {
    val lines = file.getLines().toList.zipWithIndex
    val n = lines.size
    val wonCardList = lines.foldLeft(new Array[Int](n)) {
      (ll,idS) => parseLine2(idS._2, n - 1, idS._1, ll)
    }
    wonCardList.sum
  }
}
