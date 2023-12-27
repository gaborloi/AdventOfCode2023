package org.practice.advent

import com.sun.tools.javac.code.TypeTag

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.io.BufferedSource

object Task12 {
  case class SpringMapLine(faultyLine: List[Char], insight: List[Int]) {
    @tailrec
    final def countArrangements(insightIdx: Int, indexToCount: ListMap[Int, Int]): Int = {
      val currentInsight = insight(insightIdx)
      val indexCountUpd = indexToCount.foldLeft(ListMap[Int, Int]()) { case (lm, (previousEnd, n)) =>
        val endIndex = faultyLine.zipWithIndex.find { case (c, i) =>
          (c == '#') && (i > previousEnd)
        }.map(_._2).getOrElse(faultyLine.length - currentInsight) + currentInsight - 1
        val potentialStart = faultyLine.zipWithIndex.filter { case (c, i) =>
          ((c == '#') || (c == '?')) && (i > previousEnd) && (i <= endIndex)
        }.map(_._2)

        val partialLM = potentialStart.foldLeft(ListMap[Int, Int]()) { (lm2, i) =>
          val relevantRange = i until i + currentInsight
          if (potentialStart.containsSlice(relevantRange) && faultyLine(i + currentInsight) != '#') {
            lm2.updated(i + currentInsight, n)
          } else lm2
        }
        partialLM.keys.toSet.union(lm.keys.toSet).foldLeft(ListMap[Int, Int]()) { (m, k) =>
          m.updated(k, partialLM.getOrElse(k, 0) + lm.getOrElse(k, 0))
        }
      }
      if (insightIdx == insight.length - 1) {
        val lastHashTag = faultyLine.zipWithIndex.findLast { case (c, _) => c == '#' }.map(_._2).getOrElse(-1)
        println(faultyLine, insight, indexCountUpd.map { case (k,v) => if (k > lastHashTag) v else 0 }.sum)
        return indexCountUpd.map { case (k,v) => if (k > lastHashTag) v else 0 }.sum
      }
      countArrangements(insightIdx + 1, indexCountUpd)
    }
  }

  def parse(line: String): SpringMapLine = line.split(" ") match {
    case Array(x: String, y: String) =>
      SpringMapLine((x + ".").toList, y.split(",").map(_.toInt).toList)
  }

  def calcFile1(file: BufferedSource): Int = {
    val lines = file.getLines()
    lines.foldLeft(0) { (ct, line) =>
      val springMapLine = parse(line)
      ct + springMapLine.countArrangements(0, ListMap(-1 -> 1))
    }
  }

  def calcFile2(file: BufferedSource): Long = {
    2
  }
}