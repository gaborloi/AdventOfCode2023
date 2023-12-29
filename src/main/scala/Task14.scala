package org.practice.advent

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource

object Task14 {
  type CharMatrix = Array[Array[Char]]

  def gravitate(problemLine: String): Int = {
    val ordered = problemLine.split("#").foldLeft("") { (orderedPL, subProb) =>
      s"$orderedPL#${subProb.sorted}"
    }.drop(1)
    val fixed = ordered + "#".repeat(problemLine.length - ordered.length)
    eval(fixed)
  }

  def gravitate2(problemLine: String): Array[Char] = {
    val ordered = problemLine.split("#").foldLeft("") { (orderedPL, subProb) =>
      s"$orderedPL#${subProb.sorted}"
    }.drop(1)
    (ordered + "#".repeat(problemLine.length - ordered.length)).toCharArray
  }

  def eval(gravitated: String): Int = gravitated.zipWithIndex.map { case (c, i) =>
    if (c == '1') gravitated.length - i else 0
  }.sum

  def arrayEval(cm: CharMatrix): Int = cm.map(arr => eval(arr.mkString)).sum

  def rotateByOne(in: Array[Array[Char]]): Array[Array[Char]] = {
    val size = in.length - 1
    in.indices.map { i =>
      in.head.indices.map { j =>
        in(j)(size - i)  //(0,0) -> (0, s), (s,0) -> (0,0), (0,s) -> (s,s), (s,s) -> (s,0)
      }.toArray
    }.toArray
  }
  @tailrec
  def cycle(problemCache: List[(CharMatrix, Int)], n: Int, i: Int): List[(CharMatrix, Int)] = {
    val (prob, _) = problemCache.last
    if (i + 1 == n ) {
      println("No convergence")
      return problemCache
    }
    val newProb = (0 to 3).foldLeft(prob) { (pr, _) =>
      rotateByOne(pr.map(arr => gravitate2(arr.mkString)))
    }
    val newProbVal = arrayEval(newProb)
    if (problemCache.exists { case (p, i) =>
      (i == newProbVal) && p.zip(newProb).forall {case (a, b) => a.sameElements(b) }
    }) return problemCache :+ (newProb, arrayEval(newProb))
    cycle(problemCache :+ (newProb, arrayEval(newProb)), n, i + 1)
  }
  @tailrec
  def rotateByN(n: Int, i: Int, in: Array[Array[Char]]): Array[Array[Char]] = {
    if (i == n) return in
    rotateByN(n, i + 1, rotateByOne(in))
  }
  def calcFile1(file: BufferedSource): Int = {
    val lines = file.getLines().toList
    val problem = (for {
      line <- lines
    } yield line.replace(".", "2").replace("O", "1").toCharArray).toArray

    problem.transpose.foldLeft(0) { (sumValue, charArr) =>
      sumValue + gravitate(charArr.mkString)
    }
  }

  def calcFile2(file: BufferedSource): Int = {
    val lines = file.getLines().toList
    val problem = (for {
      line <- lines
    } yield line.replace(".", "2").replace("O", "1").toCharArray).toArray.transpose

    val cache = cycle(List((problem, arrayEval(problem))), 1000, 0)

    val (lastProb, lastVal) = cache.last
    val repIdx = cache.indexWhere { case (p, i) =>
      (i == lastVal) && p.zip(lastProb).forall { case (a, b) => a.sameElements(b) }
    }
    val relevantIdx = (1000000000 - repIdx) % (cache.length - repIdx - 1)
    cache(repIdx + relevantIdx)._2
  }
}