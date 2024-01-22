package org.practice.advent

import scala.annotation.tailrec
import scala.io.BufferedSource

object Task22 {

  case class Cord3D(x: Int, y: Int, z: Int) {
    def +(that: Cord3D): Cord3D = Cord3D(x + that.x, y + that.y, z + that.z)

    def -(that: Cord3D): Cord3D = Cord3D(x - that.x, y - that.y, z - that.z)

    def *(that: Cord3D): Int = x * that.x + y * that.y + z * that.z

    def *(const: Int): Cord3D = Cord3D(x * const, y * const, z * const)
  }
  case class Brick(cord1: Cord3D, cord2: Cord3D) {
    val head: Int = math.max(cord1.z, cord2.z)
    val toe: Int = math.min(cord1.z, cord2.z)

    def fallN(n: Int): Brick = Brick(cord1 - Cord3D(0, 0, n), cord2 - Cord3D(0, 0, n))

    def checkCollision(other: Brick): Boolean = checkDim(cord1.x, cord2.x, other.cord1.x, other.cord2.x) &&
      checkDim(cord1.y, cord2.y, other.cord1.y, other.cord2.y)

  }

  def checkDim(a1: Int, a2: Int, b1: Int, b2: Int): Boolean =
    math.max(a1, a2) >= math.min(b1, b2) && math.max(b1, b2) >= math.min(a1, a2)

  class Sandbox(bricks: List[Brick]) {
    val box: List[Brick] = bricks.sortBy(_.head) // TODO could be Queue

    def fallBricks(): Sandbox = new Sandbox(fall(box.iterator, List()))

    def countRemovable(): Int = {
      box.map(_.head).toSet.foldLeft(0) { (count, currHead) =>
        count + countLevel(box.filter(_.head == currHead), box.filter(_.toe == currHead + 1))
      }
    }

    def countLevel(bricksToCheck: List[Brick], bricksAbove: List[Brick]): Int = {
//      println(bricksToCheck.size, bricksAbove.size)
      bricksToCheck.foldLeft(0) { (count, curr) =>
        val otherSupports = bricksToCheck.filterNot(_ == curr)
        val isStatic = bricksAbove.forall { ba => otherSupports.exists(b2c => ba.fallN(1).checkCollision(b2c))}
        val out = if (isStatic) {
          println(curr)
          1
        } else 0
        count + out
      }
    }
    @tailrec
    final def fall(boxIt: Iterator[Brick], fallenBricks: List[Brick]): List[Brick] = {
      val currentBrick = boxIt.next()
//      println(currentBrick)
      val (updatedBrick, stopped) = fallenBricks.foldLeft((currentBrick, false)) { case ((cB, stopped), fB) =>
        if (stopped) (cB, true) else {
          val fallenCb = cB.fallN(cB.toe - fB.head)
          if (fallenCb.checkCollision(fB)) {
            (cB.fallN(cB.toe - fB.head - 1), true)
          } else (fallenCb, false)
        }
      }
      val settledBrick = if (stopped) updatedBrick else updatedBrick.fallN(updatedBrick.toe - 1)
      println(currentBrick, settledBrick)
      val updFallenBricks = (fallenBricks :+ settledBrick).sortBy(_.head).reverse
      if (!boxIt.hasNext) return updFallenBricks
      fall(boxIt, updFallenBricks)
    }
  }

  object Sandbox {
    def parseBricks(line: Iterator[String]): Iterator[Brick] = for (l <- line) yield {
      val rawCords = l.split("~")
      val first = rawCords(0).split(",").map(_.toInt)
      val second = rawCords(1).split(",").map(_.toInt)
      Brick(Cord3D(first(0), first(1), first(2)), Cord3D(second(0), second(1), second(2)))
    }
  }

  def calcFile1(file: BufferedSource): Int = {
    val bricks = Sandbox.parseBricks(file.getLines()).toList
    val sandBox = new Sandbox(bricks)
    val fallenSB = sandBox.fallBricks()
    println(fallenSB.box)
    fallenSB.countRemovable()
  }

  def calcFile2(file: BufferedSource): Long = {
    2
  }
}