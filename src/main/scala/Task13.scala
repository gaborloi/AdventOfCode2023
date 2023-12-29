package org.practice.advent

import scala.annotation.tailrec
import scala.io.BufferedSource

object Task13 {

  type CharMatrix = Array[Array[Char]]
  case class MirrorPuzzle(mirrored: CharMatrix)
  @tailrec
  final def checkVertical(mirrored: CharMatrix, mirrorIdx: Int): Int = {
    val hasMirror = (1 to math.min(mirrorIdx + 1, mirrored.head.length - mirrorIdx - 1)).forall { i =>
      mirrored.forall( row => row(mirrorIdx - i + 1) == row(mirrorIdx + i))
    }
    if (hasMirror) return mirrorIdx + 1
    if (mirrorIdx == mirrored.head.length - 2) return 0
    checkVertical(mirrored, mirrorIdx + 1)
  }

  @tailrec
  final def checkVerticalSmudge(mirrored: CharMatrix, mirrorIdx: Int): Int = {
    val hasMirror = (1 to math.min(mirrorIdx + 1, mirrored.head.length - mirrorIdx - 1)).map { i =>
      mirrored.count(row => row(mirrorIdx - i + 1) != row(mirrorIdx + i))
    }.sum == 1
    if (hasMirror) return mirrorIdx + 1
    if (mirrorIdx == mirrored.head.length - 2) return 0
    checkVerticalSmudge(mirrored, mirrorIdx + 1)
  }

  @tailrec
  def parseFile(lineList: List[String], lmp: List[MirrorPuzzle]): List[MirrorPuzzle] = {
    val emptyIdx = lineList.indexOf("")
    if (emptyIdx == -1) return lmp :+ MirrorPuzzle(lineList.map(_.toCharArray).toArray)
    val lmpUpd = lmp :+ MirrorPuzzle(lineList.take(emptyIdx).map(_.toCharArray).toArray)
    parseFile(lineList.drop(emptyIdx + 1), lmpUpd)
  }
  def calcFile1(file: BufferedSource): Int = {
    val strList = file.getLines().toList
    val puzzles = parseFile(strList, List())
    puzzles.foldLeft(0) { (n, mp) =>
      n + checkVertical(mp.mirrored, 0) + checkVertical(mp.mirrored.transpose, 0) * 100
    }
  }

  def calcFile2(file: BufferedSource): Int = {
    val strList = file.getLines().toList
    val puzzles = parseFile(strList, List())
    puzzles.foldLeft(0) { (n, mp) =>
      n + checkVerticalSmudge(mp.mirrored, 0) + checkVerticalSmudge(mp.mirrored.transpose, 0) * 100
    }
  }
}