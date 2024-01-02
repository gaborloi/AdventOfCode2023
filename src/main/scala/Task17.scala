package org.practice.advent

import scala.annotation.tailrec
import scala.io.BufferedSource
import scala.collection.immutable.SortedSet

object Task17 {
  case class Cord(r: Int, c: Int) {
    def +(that: Cord): Cord = Cord(r + that.r, c + that.c)

    def -(that: Cord): Cord = Cord(r - that.r, c - that.c)

    def *(that: Cord): Int = r * that.r + c * that.c

    def valid(maxRowIdx: Int, maxColIdx: Int): Boolean = (r <= maxRowIdx) && (c <= maxColIdx) && (r > -1) && (c > -1)
  }

  object Cord {
    val STEPS: List[Cord] = List(Cord(1, 0), Cord(0, 1), Cord(-1, 0), Cord(0, -1))
  }
  case class HeatMap(mapOfHeat: Array[Array[Int]]) {
    val maxRowIdx: Int = mapOfHeat.length - 1
    val maxColIdx: Int = mapOfHeat.head.length - 1

    val optPathHist: Array[Array[Map[Cord, Int]]] = mapOfHeat.map(_.map( _ => Map[Cord, Int]()).toArray)

    @tailrec
    final def backwardPropagation(optPaths: List[OptPathStep]): List[OptPathStep] = {
//      println(optPaths)
      val nextIter = optPaths.head

      if ((nextIter.cord.r == 0) && (nextIter.cord.c == 0)) return List(nextIter)
      val currentHist = optPathHist(nextIter.cord.r)(nextIter.cord.c)
      val nextSteps = nextIter.update(currentHist, nextIter.value)

      backwardPropagation((optPaths.drop(1) ++ nextSteps).sortBy(_.utilityValue).distinct)
    }

    @tailrec
    final def forwardPropagation(cord: Cord, currentValue: Int): Cord = {
      if ((cord.r == maxRowIdx) && (cord.c == maxColIdx)) return cord
      val currentDir = optPathHist(cord.r)(cord.c).filter { case (_, value) => value == currentValue }.head._1
      val thisValue = mapOfHeat(cord.r)(cord.c)
//      println(cord, thisValue)
      val stdDir = Cord(currentDir.r/math.max(1,math.abs(currentDir.r)), currentDir.c/math.max(1,math.abs(currentDir.c)))
      forwardPropagation(cord + stdDir, currentValue - thisValue)
    }

    case class OptPathStep(cord: Cord, value: Int) {

      val utilityValue: Int = value
      def eligibleSteps(optPathForward: Cord): List[Cord] = Cord.STEPS.flatMap { stp =>
        optPathForward + stp match {
          case out if math.max(out.r, out.c) > 3 => None
          case _ if optPathForward * stp < 0 => None
          case _ => Some(stp)
        }
      }

      def update(optForwards: Map[Cord, Int], currentValue: Int): List[OptPathStep] = {
        optForwards.filter(_._2 == currentValue).flatMap { case (c, _) =>
          eligibleSteps(c).flatMap { stp =>
            val newCord = cord - stp
            if (!newCord.valid(maxRowIdx, maxColIdx)) {
              None
            } else {
              val optPath = if (c * stp > 0) c + stp else stp
              val optPathMap = optPathHist(newCord.r)(newCord.c)
              val newValue = currentValue + mapOfHeat(newCord.r)(newCord.c)
              val hasBetterPath = (optPathMap.count {
                case (differentDir, v) if (differentDir * optPath <= 0) && (differentDir * differentDir == 1) => v < newValue
                case _ => false
              } >= 2) || optPathMap.exists {
                case (sameDir, v) => (v < newValue) && (math.abs(sameDir.r) <= math.abs(optPath.r)) && (math.abs(sameDir.c) <= math.abs(optPath.c))
                case _ => false
              }
              if (hasBetterPath) None else {
                optPathHist(newCord.r)(newCord.c) = optPathMap.updated(optPath, newValue)
                Some(OptPathStep(newCord, newValue))
              }
            }
          }
        }.toList
      }
    }
  }

  def calcFile1(file: BufferedSource): Int = {
    val lines = file.getLines().toList
    val heatMap: HeatMap = HeatMap((for {
      line <- lines
    } yield line.map(_.toString.toInt).toArray).toArray)

    val startValue = heatMap.mapOfHeat(heatMap.maxRowIdx)(heatMap.maxColIdx)
    heatMap.optPathHist(heatMap.maxRowIdx)(heatMap.maxColIdx) =
      heatMap.optPathHist(heatMap.maxRowIdx)(heatMap.maxColIdx).updated(Cord(0,0), startValue)
    val res = heatMap.backwardPropagation(
      List(heatMap.OptPathStep(Cord(heatMap.maxRowIdx, heatMap.maxColIdx), startValue))
    ).head

//    heatMap.forwardPropagation(res.cord, res.value)

    res.value  - heatMap.mapOfHeat(0)(0)
  }

  def calcFile2(file: BufferedSource): Int = {
    2
  }
}