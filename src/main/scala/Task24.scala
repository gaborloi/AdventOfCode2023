package org.practice.advent

import org.apache.commons.math3.exception.{ConvergenceException, MathArithmeticException}

import scala.io.BufferedSource
import org.apache.commons.math3.linear.{Array2DRowFieldMatrix, ArrayFieldVector, FieldLUDecomposition}
import org.apache.commons.math3.util.BigReal

import scala.annotation.tailrec
import scala.math.BigDecimal.{int2bigDecimal, javaBigDecimal2bigDecimal}

object Task24 {

  def abs(d: BigDecimal): BigDecimal = if (d < 0) -d else d

  class GradientDescent(winds: List[WindInfoBD], learningRate: BigDecimal, eps: BigDecimal) {

    val paramcount: Int = winds.head.position.length + winds.head.velocity.length

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

    def loss(k: WindInfoBD): BigDecimal = winds.foldLeft(BigDecimal(0)) { (s, w) =>
      val t = calcTime(w.position, w.velocity, k.position, k.velocity)
      s + dist(w.calcCord(t), k.calcCord(t))
    }

    def gradient(k: WindInfoBD): List[BigDecimal] = {
      val origLoss = loss(k)
      (for (i <- 0 until paramcount) yield (loss(k.perturb(i, eps)) - origLoss) / eps).toList
    }

    @tailrec
    final def gradientDescent(current: WindInfoBD, currIdx: Int, maxIdx: Int): WindInfoBD = {
      val grad = gradient(current)
//      println(current, grad)
      val listOfParam = (current.position ++ current.velocity).zip(grad).map { case (par, g) => par - learningRate * g }
      val nextWind = WindInfoBD(listOfParam.take(current.dim), listOfParam.drop(current.dim))
      val nextLoss = loss(nextWind)
//      println(s"$currIdx: $nextLoss, $nextWind")
      if ( nextLoss < 0.001) return nextWind
      if (currIdx > maxIdx) {
        println(s"$currIdx: $nextLoss, $nextWind")
//        throw new ConvergenceException()
        return nextWind
      }
      gradientDescent(nextWind, currIdx + 1, maxIdx)
    }
  }

  case class WindInfoBD(position: List[BigDecimal], velocity: List[BigDecimal]) {
    val dim: Int = position.length
    def perturb(i: Int, eps: BigDecimal): WindInfoBD = {
      val listOfParam = (position ++ velocity).zipWithIndex.map { case (par, j) =>
        if (i == j) par + eps else par
      }
      WindInfoBD(listOfParam.take(dim), listOfParam.drop(dim))
    }

    def calcCord(t: BigDecimal): List[BigDecimal] = position.zip(velocity).map { case (p,v) => p + v * t }

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
    val g = new GradientDescent(winds.take(3), 0.1, 0.01)
//    println(g.loss(WindInfoBD(List(24,13,10), List(-3,1,2))))
    val wTest = WindInfoBD(List(26, 12, 11), List(-4, 1, 3))
//    val rock = g.gradientDescent(wTest, 0, 1000)
//    rock.position.sum
//    println(g.gradient(wTest))
//    println(g.loss(wTest))
//    println(g.loss(wTest.perturb(3, 1)))
    var minLoss = g.loss(wTest)
    var minRock = wTest
    winds foreach { w =>
      val rock = g.gradientDescent(w, 0, 1000)
      val rockLoss = g.loss(rock)
      if (rockLoss < minLoss) {
        minRock = rock
        minLoss = rockLoss
        println(rock, rockLoss)
      }
    }
    minRock.position.sum
//    1
  }
}