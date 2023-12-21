package org.practice.advent

import org.practice.advent.Task4.parseLine

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource

object Task6 {
  def EPS = 0.01
  case class Race(time: Long, record: Long) {
    def dist(chargeTime: Long) = chargeTime * math.min(time - chargeTime, 0)

    def recordCount(): Long = {
      val d = math.sqrt(time * time - 4 * record)
      val x1 = (time - d)/2.0
      val x2 = (time + d)/2.0
      println(x1, x2)
      (x2 - EPS).toLong - (x1 + EPS).toLong
    }
  }

  def parseFile1(file: BufferedSource): Long = {
    val lines = file.getLines().toList
    val times = lines.head.substring(11).trim.split(" +").toList
    val records = lines(1).substring(11).trim.split(" +").toList
    val races: List[Race] = times.zip(records).map { r => Race(r._1.toLong, r._2.toLong)}
    races.foldLeft(1L) { (p ,r) => println(r.recordCount())
      p * r.recordCount() }
  }

  def parseFile2(file: BufferedSource): Long = {
    val lines = file.getLines().toList
    val times = List(lines.head.substring(11).replace(" ", ""))
    val records = List(lines(1).substring(11).replace(" ", ""))
    val races: List[Race] = times.zip(records).map { r => Race(r._1.toLong, r._2.toLong)}

    races.foldLeft(1L) { (p ,r) => println(r.recordCount())
      p * r.recordCount() }
  }
}