package org.practice.advent

import scala.annotation.tailrec
import scala.io.BufferedSource

object Task10 {
//  type Cord = (Char)
  case class PipeMap(mapArray: Array[Array[Char]]) {
    def startingPos(): (Int, Int, Char) = {
      for (r <- mapArray.indices; c <- mapArray.head.indices) {
        if (mapArray(r)(c) == 'S') {
          if (mapArray(math.max(0, r-1))(c) != '.')
            return (r - 1, c, 'd')
          else if (mapArray(math.min(mapArray.length - 1, r + 1))(c) != '.')
            return (r + 1, c, 'u')
          else if (mapArray(r)(math.max(c - 1, 0)) != '.')
            return (r, c + 1, 'r')
          else if (mapArray(r)(math.min(c + 1, mapArray.head.length - 1)) != '.')
            return (r, c - 1, 'l')
        }
      }
      (-1, -1, 'x': Char)
    }
    @tailrec
    final def iterateThrough(rowIdx: Int, colIdx: Int, dirFrom: Char, iterCount: Int): Int = {
      mapArray(rowIdx)(colIdx) match {
        case 'S' => iterCount
        case '|' => dirFrom match {
          case 'u' => iterateThrough(rowIdx + 1, colIdx, 'u', iterCount + 1)
          case 'd' => iterateThrough(rowIdx - 1, colIdx, 'd', iterCount + 1)
        }
        case '-' => dirFrom match {
          case 'l' => iterateThrough(rowIdx, colIdx + 1, 'l', iterCount + 1)
          case 'r' => iterateThrough(rowIdx, colIdx - 1, 'r', iterCount + 1)
        }
        case 'L' => dirFrom match {
          case 'u' => iterateThrough(rowIdx, colIdx + 1, 'l', iterCount + 1)
          case 'r' => iterateThrough(rowIdx - 1, colIdx, 'd', iterCount + 1)
        }
        case 'J' => dirFrom match {
          case 'u' => iterateThrough(rowIdx, colIdx - 1, 'r', iterCount + 1)
          case 'l' => iterateThrough(rowIdx - 1, colIdx, 'd', iterCount + 1)
        }
        case '7' => dirFrom match {
          case 'l' => iterateThrough(rowIdx + 1, colIdx, 'u', iterCount + 1)
          case 'd' => iterateThrough(rowIdx, colIdx - 1, 'r', iterCount + 1)
        }
        case 'F' => dirFrom match {
          case 'r' => iterateThrough(rowIdx + 1, colIdx, 'u', iterCount + 1)
          case 'd' => iterateThrough(rowIdx, colIdx + 1, 'l', iterCount + 1)
        }
      }
    }

    def iterateFlow(rowIdx, colIdx, )
  }
  def calcFile1(file: BufferedSource): Int = {
    val lines = file.getLines().toList
    val charArray = ( for {
      line <- lines
    } yield line.toCharArray).toArray
//    for (arr <- charArray) println(arr.toList)
    val pipeMap = PipeMap(charArray)
    val start = pipeMap.startingPos()
    (pipeMap.iterateThrough(start._1, start._2, start._3, 0) + 1) / 2
  }

  def calcFile2(file: BufferedSource): Int = {
    2
  }
}