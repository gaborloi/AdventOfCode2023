package org.practice.advent

import org.practice.advent.Task4.parseLine

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource

object Task6 {
  def EPS = 0.01
  case class Race(time: Int, record: Int) {
    def dist(chargeTime: Int) = chargeTime * math.min(time - chargeTime, 0)

    def recordCount(): Int = {
      val d = math.sqrt(time * time - 4 * record)
      val x1 = (time - d)/2.0
      val x2 = (time + d)/2.0
      (x2 - EPS).toInt - (x1 + EPS).toInt
    }
  }

  def parseFile1(file: BufferedSource): Int = {
    val lines = file.getLines().toList
    val times = lines.head.substring(11).split(" +").toList
    val records = lines(1).substring(11).split(" +").toList
    val races: List[Race] = times.zip(records).map { r => Race(r._1.toInt, r._2.toInt)}
    races.foldLeft(1) { (p ,r) => println(r.recordCount())
      p * r.recordCount() }
  }
}