package org.practice.advent

import scala.annotation.tailrec
import scala.io.BufferedSource
import scala.util.matching.Regex


object Task19 {

  implicit class RangeImprovements(r: Range) {
    def myIntersect(that: Range): Range = Range.inclusive(math.max(r.min, that.min), math.min(r.max, that.max))
  }
  case class Xmas(x: Int, m: Int, a: Int, s: Int) {
    @tailrec
    final def calcXmas(ruleName: String, ruleSet: Map[String, List[Rule]]): Int = {
      val nextStep = ruleSet(ruleName).find( rule => rule.eval(this) != "").get.exit

      nextStep match {
        case "A" => sum()
        case "R" => 0
        case _ => calcXmas(nextStep, ruleSet)
      }
    }

    def sum(): Int = x + m + a + s
  }

  case class XmasRange(x: Range, m: Range, a: Range, s: Range) {
    final def calcXmasRange(ruleName: String, ruleSet: Map[String, List[Rule]]): Long = {
      val (_, count) = ruleSet(ruleName).foldLeft((this, 0L)) { case ((nextXmasRange, sumCount), rule) =>

        val (updXMRTrue, nextStepTrue) = rule.eval(direction = true, nextXmasRange)

        val trueCount = if(updXMRTrue.isEmpty) 0L else nextStepTrue match {
          case "A" => updXMRTrue.prod
          case "R" => 0L
          case _ => updXMRTrue.calcXmasRange(nextStepTrue, ruleSet)
        }

        val (updXMRFalse, _) = rule.eval(direction = false, nextXmasRange)
        (updXMRFalse, sumCount + trueCount)
      }
      count
    }

    def updateRange(property: Char, r: Range): XmasRange = property match {
        case 'x' => XmasRange(x myIntersect r, m, a, s)
        case 'm' => XmasRange(x, m myIntersect r, a, s)
        case 'a' => XmasRange(x, m, a myIntersect r, s)
        case 's' => XmasRange(x, m, a, s myIntersect r)
    }

    def prod: Long = x.size.toLong * m.size.toLong * a.size.toLong * s.size.toLong

    def isEmpty: Boolean = x.isEmpty || m.isEmpty || a.isEmpty || s.isEmpty
  }
  object Xmas {
    val regExp: Regex = "\\{x=(\\d*),m=(\\d*),a=(\\d*),s=(\\d*)}".r
  }

  case class Rule(input: Char, largerThan: Boolean, value: Int, exit: String) {
    def eval(xmas: Xmas): String = {
      val decision = input match {
        case 'x' => if (largerThan) xmas.x > value else xmas.x < value
        case 'm' => if (largerThan) xmas.m > value else xmas.m < value
        case 'a' => if (largerThan) xmas.a > value else xmas.a < value
        case 's' => if (largerThan) xmas.s > value else xmas.s < value
        case 'Q' => true
      }
      if (decision) exit else ""
    }

    def eval(direction: Boolean, xmas: XmasRange): (XmasRange, String) = {
      val ruleRange = if (largerThan)
        if(direction)
          Range.inclusive(value + 1, 4000)
        else Range.inclusive(1, value)
      else
        if(direction)
          Range(1, value)
        else Range.inclusive(value, 4000)
      val newXmas = if (input == 'Q') xmas else xmas.updateRange(input, ruleRange)

      (newXmas, if (direction) exit else "")
    }
  }

  def parseRules(rawLine: String): (String, List[Rule]) = {
    val separated = rawLine.dropRight(1).split('{')
    separated.head -> separated(1).split(',').map( rawRule => {
      if (!rawRule.contains(':')) {
        Rule('Q', largerThan = true, 0, rawRule)
      } else {
        val sepValAndExit = rawRule.substring(2).split(':')
        Rule(rawRule(0), rawRule(1) == '>', sepValAndExit(0).toInt, sepValAndExit(1))
      }
    }).toList
  }

  def parseXmas(rawLine: String): Xmas = {
    val parsed = Xmas.regExp.findFirstMatchIn(rawLine).get
    Xmas(parsed.group(1).toInt, parsed.group(2).toInt, parsed.group(3).toInt, parsed.group(4).toInt)
  }

  def calcFile1(file: BufferedSource): Int = {
    val lines = file.getLines().toList
    val emptyLineIdx = lines.indexOf("")
    val rulesLines = lines.take(emptyLineIdx).map(parseRules).toMap
    val xmases = lines.slice(emptyLineIdx + 1, lines.length).map(parseXmas)
    xmases.map(_.calcXmas("in", rulesLines)).sum
  }

  def calcFile2(file: BufferedSource): Long = {
    val lines = file.getLines().toList
    val emptyLineIdx = lines.indexOf("")
    val rulesLines = lines.take(emptyLineIdx).map(parseRules).toMap
    XmasRange(
      Range.inclusive(1, 4000), Range.inclusive(1, 4000) , Range.inclusive(1, 4000), Range.inclusive(1, 4000)
    ).calcXmasRange("in", rulesLines)
  }
}