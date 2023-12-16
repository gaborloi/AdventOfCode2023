package org.practice.advent
import scala.annotation.tailrec
import scala.io.BufferedSource
import scala.util.matching.Regex

case class Task1Res(subString:String, foundStr: String)
object Task1 {
  private val lastNumberPattern: Regex = "(\\d)\\D*$".r
  private val firstNumberPattern: Regex = "^\\D*(\\d)".r

  private val spelled = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  private val spelledLastPatterns = lastNumberPattern +: spelled.map { s => ("(" + s + ")\\D*$").r }
  private val spelledFirstPatterns = firstNumberPattern +: spelled.map {s => ("^\\D*(" + s + ")").r }
  def twoDigitGetter(input: String): Int = {

    firstNumberPattern.findFirstMatchIn(input) match {
      case Some(m1) =>
        lastNumberPattern.findFirstMatchIn(input) match {
          case Some(m2) => (m1.group(1) + m2.group(1)).toIntOption.getOrElse(0)
        }
      case None => 0
    }
  }

  @tailrec
  def patternHandler(res: Task1Res, pattern: Regex, isFirst: Boolean): Task1Res = {
    pattern.findFirstMatchIn(res.subString) match {
      case Some(m) => if (isFirst) {
        println(m.group(0), m.group(1))
        patternHandler(Task1Res(m.group(0), m.group(1)), (pattern + ".").r, isFirst)
      } else {
        patternHandler(Task1Res(m.group(0), m.group(1)), ("." + pattern).r, isFirst)
      }
      case None => res
    }
  }

  def twoDigitSpelled(input: String): Int = {
    val lastRes = spelledLastPatterns.foldLeft(Task1Res(input, "")) { (n, r) =>
      patternHandler(n, r, isFirst = false)
    }
    val firstRes = spelledFirstPatterns.foldLeft(Task1Res(input, "")) { (n, r) =>
      patternHandler(n, r, isFirst = true)
    }

    val lastInt = lastRes.foundStr.toIntOption match {
      case Some(n) => n
      case None => spelled.indexOf(lastRes.foundStr)
    }

    val firstInt = firstRes.foundStr.toIntOption match {
      case Some(n) => n
      case None => spelled.indexOf(firstRes.foundStr)
    }
    println(s"$input: ${10 * firstInt + lastInt}")
    math.max(10 * firstInt + lastInt, 0)
  }

  def parseFile(file: BufferedSource): Int = file.getLines().foldLeft(0) {
    (n,s) => twoDigitGetter(s) + n
  }

  def parseFile2(file: BufferedSource): Int = file.getLines().foldLeft(0) {
    (n,s) => twoDigitSpelled(s) + n
  }
}
