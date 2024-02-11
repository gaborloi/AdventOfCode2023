package org.practice.advent

import org.apache.commons.math3.exception.MathArithmeticException

import scala.io.BufferedSource
import org.apache.commons.math3.linear.{Array2DRowFieldMatrix, ArrayFieldVector, FieldLUDecomposition}
import org.apache.commons.math3.util.BigReal

import scala.annotation.tailrec
import scala.math.BigDecimal.{int2bigDecimal, javaBigDecimal2bigDecimal}

object Task24 {

  def abs(d: BigDecimal): BigDecimal = if (d < 0) -d else d

  class GradientDescent(val w1: WindInfoBD, val w2: WindInfoBD, w3: WindInfoBD, learningRate: BigDecimal, eps: BigDecimal) {

    def calcTime(pw: List[BigDecimal], vw: List[BigDecimal], pk: List[BigDecimal], vk: List[BigDecimal]): BigDecimal = {
      val dp = pw.zip(pk).map { case (p1, p2) => p1 - p2 }
      val dv = vw.zip(vk).map { case (v1, v2) => v1 - v2 }
      val dpdv = dp.zip(dv).map { case (dpi, dvi) => dpi * dvi }
      dv.map(_.pow(2)).sum match {
        case zero if zero == 0 => 0
        case sdv2 => - dpdv.sum / sdv2
      }
    }

    def dist(c1: List[BigDecimal], c2: List[BigDecimal]): BigDecimal =
      c1.zip(c2).map { case (c1i, c2i) => (c1i-c2i).pow(2) }.sum

    def loss(k: WindInfoBD): BigDecimal = {
      val t = calcTime(w3.position, w3.velocity, k.position, k.velocity)
      dist(w3.calcCord(t), k.calcCord(t))
    }

    def gradient(rock: WindInfoBD, t1: BigDecimal, t2: BigDecimal): (BigDecimal, BigDecimal) = {
      val origLoss = loss(rock)
      (loss(w1.createRock(w2, t1 + eps, t2)) - origLoss, loss(w1.createRock(w2, t1, t2 + eps)) - origLoss)
    }

    @tailrec
    final def gradientDescent(currRock: WindInfoBD, currT1: BigDecimal, currT2: BigDecimal, currIdx: Int, maxIdx: Int): WindInfoBD = {
      val grad = gradient(currRock, currT1, currT2)
      val t1Upd = (currT1 - learningRate * grad._1).max(0)
      val t2Upd = (currT2 - learningRate * grad._2).max(0)
      val nextRock = w1.createRock(w2, t1Upd, t2Upd)
      val nextLoss = loss(nextRock)
      if ( nextLoss < 0.001) return nextRock
      if (currIdx > maxIdx) {
        println(s"$currIdx: $nextLoss, $nextRock")
        return nextRock
      }
      gradientDescent(nextRock, t1Upd, t2Upd, currIdx + 1, maxIdx)
    }
  }

  case class WindInfoBD(position: List[BigDecimal], velocity: List[BigDecimal]) {

    def createRock(w2: WindInfoBD, t1: BigDecimal, t2: BigDecimal): WindInfoBD = {
      val dt = t2 - t1
      val cord1 = calcCord(t1)
      val rockVelocity: List[BigDecimal] = if (dt == 0) {
        w2.velocity.map ( _ => 0 )
      } else w2.calcCord(t2).zip(cord1).map { case (c2, c1) => (c2 - c1) / dt }
      val rockPosition = cord1.zip(rockVelocity).map { case (c, v) => c - v * t1 }
      WindInfoBD(rockPosition, rockVelocity)
    }

    def calcCord(t: BigDecimal): List[BigDecimal] = position.zip(velocity).map { case (p, v) => p + v * t }
  }

  case class WindInfo(position: Array[BigReal], velocity: Array[BigReal]) {
    def calculate2D(wI: WindInfo, areaMin: BigDecimal, areaMax: BigDecimal): Int = {
      val A = new Array2DRowFieldMatrix[BigReal](
        Array(velocity, wI.velocity.map { _.multiply(-1) })
      ).getSubMatrix(0, 1, 0, 1).transpose()
      val bRaw = position.zip(wI.position).map { case (b1, b2) => b2.subtract(b1) }
      val b = new ArrayFieldVector[BigReal](bRaw).getSubVector(0,2)
      val solver = new FieldLUDecomposition[BigReal](A).getSolver
      val t = try { solver.solve(b) } catch {
        case _: MathArithmeticException => new ArrayFieldVector[BigReal](Array(new BigReal(-1), new BigReal(-1)))
      }

      val allpos = t.toArray.forall(_.bigDecimalValue() > 0)

      if (!allpos) 0 else {
        val inArea = position.zip(velocity).map {
          case (p, v) => p.add(v.multiply(t.getEntry(0)))
        }.dropRight(1).forall { p =>
          (p.bigDecimalValue() >= areaMin) && (p.bigDecimalValue() <= areaMax)
        }
        if (inArea) 1 else 0
      }
    }
  }

  def calcFile1(file: BufferedSource, area_min: Long, area_max: Long): Int = {
    val areaMax = BigDecimal(area_max)
    val areaMin = BigDecimal(area_min)
    val winds = file.getLines().map{ line =>
      val parts = line.replaceAll(" ", "").split("@")
      val positions = parts(0).split(",").map( p => new BigReal(p.toLong))
      val velocities = parts(1).split(",").map( p => new BigReal(p.toLong))
      WindInfo(positions, velocities)
    }
    winds.toList.combinations(2).toList.foldLeft(0) { case (sum, lw) =>
      sum + lw.head.calculate2D(lw(1), areaMin, areaMax)
    }
  }

  def calcFile2(file: BufferedSource): BigDecimal = {
    val winds = file.getLines().map { line =>
      val parts = line.replaceAll(" ", "").split("@")
      val positions = parts(0).split(",").map(p => BigDecimal(p.toLong))
      val velocities = parts(1).split(",").map(p => BigDecimal(p.toLong))
      WindInfoBD(positions.toList, velocities.toList)
    }.toList

    val wTest = WindInfoBD(List(26, 12, 11), List(-4, 1, 3))
    val g0 = new GradientDescent(winds(1), winds(2), winds.head, 0.1, 0.01)
    var minLoss = g0.loss(wTest)
    var minRock = wTest
    (2 until winds.size) foreach { i =>
      val g = new GradientDescent(winds(i-1), winds(i), winds.head, 0.1, 0.01)
      val init = g.w1.createRock(g.w2, 1, 2)
      val rock = g.gradientDescent(init, 1, 2, 0, 1000)
      val rockLoss = g.loss(rock)
      if (rockLoss < minLoss) {
        minRock = rock
        minLoss = rockLoss
        println(rockLoss)
      }
    }

    (2 until winds.size) foreach { i =>
      val g = new GradientDescent(winds(i), winds(i - 1), winds.head, 0.1, 0.01)
      val init = g.w1.createRock(g.w2, 1, 2)
      val rock = g.gradientDescent(init, 1, 2, 0, 1000)
      val rockLoss = g.loss(rock)
      if (rockLoss < minLoss) {
        minRock = rock
        minLoss = rockLoss
        println(rockLoss)
      }
    }
    println(minLoss, minRock)
//    val res = WindInfoBD(List(318090941338468L, 124187623124113L, 231363386790708L), List(-78, 269, 71))
//    println(g0.loss(res))
    minRock.position.sum.toLong
  }
}