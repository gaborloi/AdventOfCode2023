package org.practice.advent

import scala.io.BufferedSource

object Task9 {

  def extrapolate(sequence: List[Int]): Int = {
    if (sequence.forall (_ == 0)) {
      return 0
    }
    extrapolate(sequence.drop(1).zip(sequence.dropRight(1)).map { case (a,b) => a - b}) + sequence.last
  }

  def backxtrapolate(sequence: List[Int]): Int = {
    if (sequence.forall (_ == 0)) {
      return 0
    }
    sequence.head - backxtrapolate(sequence.drop(1).zip(sequence.dropRight(1)).map { case (a,b) => a - b})
  }
  def calcFile1(file: BufferedSource): Int = {
    val sequences = file.getLines().map (line => line.split(" ").map(_.toInt).toList).toList
    sequences.foldLeft(0) { (sums, seq) => sums + extrapolate(seq) }
  }

  def calcFile2(file: BufferedSource): BigInt = {
    val sequences = file.getLines().map (line => line.split(" ").map(_.toInt).toList).toList
    sequences.foldLeft(0) { (sums, seq) => sums + backxtrapolate(seq) }
  }
}