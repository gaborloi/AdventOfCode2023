package org.practice.advent

import scala.collection.immutable.ListMap
import scala.io.BufferedSource

object Task15 {

  def asciiHash(strSeq: String): Int = strSeq.toCharArray.map(_.toInt).foldLeft(0) {
    (currentVal, asciiCode) => ((currentVal + asciiCode) * 17) % 256
  }
  def calcFile1(file: BufferedSource): Int = {
    val initSequence = file.getLines().next().split(",")
    initSequence.map(asciiHash).sum
  }

  def calcPower(hash: Int, idx: Int, focalLength: Int): Int = (hash + 1) * (idx + 1) * focalLength


  def calcFile2(file: BufferedSource): Int = {
    val initSequence = file.getLines().next().split(",")
    initSequence.foldLeft(Map[Int, ListMap[String, Int]]()) { (boxes, operation) =>
      if (operation.contains('=')) {
        val parts = operation.split('=')
        val hash = asciiHash(parts(0))
        val box = boxes.getOrElse(hash, ListMap()).updated(parts(0), parts(1).toInt)
        boxes.updated(hash, box)
      } else {
        val label = operation.dropRight(1)
        val hash = asciiHash(label)
        val box = boxes.getOrElse(hash, ListMap()).removed(label)
        boxes.updated(hash, box)
      }
    }.map { case (hash, lm) =>
      lm.zipWithIndex.map { case ((_, focalLength), idx) => calcPower(hash, idx, focalLength) }.sum }.sum
  }
}