package org.practice.advent

import scala.io.BufferedSource

object Task7 {
  val mapOfChars = List('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A').zipWithIndex.toMap

  val mapOfCharsJoker = List('J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A').zipWithIndex.toMap
  case class CamelHand(hand: String, bid: Int) {
    val intRepr: String = hand.groupBy(identity).map { case (_, s) => s.length }.toList.sorted.foldLeft("") {
      (s, b) => s + b.toString
    }

    val value: Int = {

      val residual = hand.zipWithIndex.foldLeft(0) { case (s, (c, i)) => s + (mapOfChars(c) + 1) * math.pow(13, 5 - i).toInt }
      val value = intRepr match {
        case "11111" => 0
        case "1112" => 1
        case "122" => 2
        case "113" => 3
        case "23" => 4
        case "14" => 5
        case "5" => 6
      }
      residual + value * math.pow(13, 6).toInt
    }
  }

  case class CamelHandJoker(hand: String, bid: Int) {
    val intRepr: String = {
      val jokerRemoved = hand.replace("J", "")
      val jokerCount = hand.length - jokerRemoved.length
      val prejoker = jokerRemoved.groupBy(identity).map { case (_, s) => s.length }.toList.sorted
      val jokerAdded = prejoker.zipWithIndex.map { case (l, i) => if ((i + 1) == prejoker.size) l + jokerCount else l }
      jokerAdded.foldLeft("") {
        (s, b) => s + b.toString
      }
    }

    val value: Int = {

      val residual = hand.zipWithIndex.foldLeft(0) { case (s, (c, i)) =>
        s + (mapOfCharsJoker(c) + 1) * math.pow(13, 5 - i).toInt
      }
      val value = intRepr match {
        case "11111" => 0
        case "1112" => 1
        case "122" => 2
        case "113" => 3
        case "23" => 4
        case "14" => 5
        case "5" => 6
        case "" => 6
      }
      residual + value * math.pow(13, 6).toInt
    }
  }

  def calcFile1(file: BufferedSource): Int = {
    val hands = file.getLines().map { line =>
      val splitted = line.split(" ")
      CamelHand(splitted(0), splitted(1).toInt)
    }.toList
    println(hands.sortBy(_.value).map {c => (c.hand, c.bid, c.value) })
    hands.sortBy(_.value).zipWithIndex.foldLeft(0) { case (s, (h,i)) => s + h.bid * (i + 1) }
  }

  def calcFile2(file: BufferedSource): Int = {
    val hands = file.getLines().map { line =>
      val splitted = line.split(" ")
      CamelHandJoker(splitted(0), splitted(1).toInt)
    }.toList
    println(hands.sortBy(_.value).map {c => (c.hand, c.bid, c.value) })
    hands.sortBy(_.value).zipWithIndex.foldLeft(0) { case (s, (h,i)) => s + h.bid * (i + 1) }
  }
}