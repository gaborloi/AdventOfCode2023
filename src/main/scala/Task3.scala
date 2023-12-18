package org.practice.advent

import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource

object Task3 {
  case class Info(strRepr: String, stIdx: Int, isSymbol: Boolean, var used: Boolean)

  case class CountedProduct(count: Int, product: Int) {
    def sumCP(cp: CountedProduct): CountedProduct = {
      CountedProduct(count + cp.count, product * cp.product)
    }
  }

  object CountedProduct {
    def zeroCP: CountedProduct = CountedProduct(0, 1)
  }

  def readline(line: String): ListBuffer[Info] = {
    var numSt = ""
    var idx: Int = 0
    var wasDigit = false
    val numbers = ListBuffer[Info]()
    for ((i, c) <- (0 to line.length) zip line) {
      if (c == '.') {
        if (wasDigit) {
          numbers += Info(numSt, idx, isSymbol = false, used = false)
          numSt = ""
          wasDigit = false
        }
      } else {
        if (c.isDigit) {
          numSt += c
          if (!wasDigit) {
            wasDigit = true
            idx = i
          }
        } else {
          numbers += Info(c.toString, i, isSymbol = true, used = false)
          if (wasDigit) {
            numbers += Info(numSt, idx, isSymbol = false, used = false)
            numSt = ""
            wasDigit = false
          }
        }
      }
    }
    if (wasDigit) {
      numbers += Info(numSt, idx, isSymbol = false, used = false)
    }
    numbers
  }

  def parseFile1(file: BufferedSource): Int = {
    var sumNum = 0
    val lines = file.getLines().map(readline).toList
    for (i <- lines.indices) {
      for (info <- lines(i)) {
        if (info.isSymbol) {
          if (i > 0) {
            sumNum += checkLine(info.stIdx, lines(i - 1))
          }
          sumNum += checkLine(info.stIdx, lines(i))
          if (i < lines.length - 1) {
            sumNum += checkLine(info.stIdx, lines(i + 1))
          }
        }
      }
    }
    lines.map(println)
    sumNum
  }

  def checkLine(sIdx: Int, lines: ListBuffer[Info]): Int = {
    lines.foldLeft(0) { (sum, prevInfo) =>
      if (!prevInfo.used && !prevInfo.isSymbol) {
        val d = checkInfo(sIdx, prevInfo)
        sum + d
      } else sum
    }
  }

  def checkInfo(s: Int, info: Info): Int =
    if ((!info.used) && (s >= info.stIdx - 1) && (s <= info.stIdx + info.strRepr.length)) {
      info.used = true
      info.strRepr.toInt
    } else 0

  def parseFile2(file: BufferedSource): Int = {
    var sumNum = 0
    val lines = file.getLines().map(readline).toList
    for (i <- lines.indices) {
      for (info <- lines(i)) {
        if (info.strRepr == "*") {
          var retPair = CountedProduct.zeroCP
          if (i > 0) {
            retPair = retPair.sumCP(checkLineCount(info.stIdx, lines(i - 1)))
          }
          retPair = retPair.sumCP(checkLineCount(info.stIdx, lines(i)))
          if (i < lines.length - 1) {
            retPair = retPair.sumCP(checkLineCount(info.stIdx, lines(i + 1)))
          }
          if (retPair.count == 2) sumNum += retPair.product
        }
      }
    }
    lines.map(println)
    sumNum
  }

  def checkLineCount(sIdx: Int, lines: ListBuffer[Info]): CountedProduct = {
    lines.foldLeft(CountedProduct.zeroCP) { (sumCP, prevInfo) =>
      if (!prevInfo.used && !prevInfo.isSymbol) {
        val d = checkInfo(sIdx, prevInfo)
        val cp = CountedProduct(math.min(1, d), math.max(1, d))
        sumCP.sumCP(cp)
      } else sumCP
    }
  }
}
