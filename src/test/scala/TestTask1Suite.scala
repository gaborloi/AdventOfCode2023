package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source
import scala.util.matching.Regex

class TestTask1Suite extends AnyFunSuite {
  val examples: Seq[String] = List(
    "1abc2",
    "pqr3stu8vwx",
    "a1b2c3d4e5f",
    "treb7uchet"
  )

  test("First case works") {
    println(Task1.twoDigitGetter(examples(3)))
  }

  test("allfile") {
    println(Task1.parseFile(Source.fromResource("input1_1.txt")))
  }

  test("Task1_2 edge cases") {
    println(Task1.twoDigitSpelled("twoonetwoonetwoone"))
  }

  test("Task1_2 test file") {
    println(Task1.parseFile2(Source.fromResource("input_test_1_2.txt")))
  }

  test("Task1_2 all") {
    println(Task1.parseFile2(Source.fromResource("input1_1.txt")))
  } //55686

  test("Task1_2 edge cases2") {
    println(Task1.twoDigitSpelled("threeight"))
  }

  test("Task1_2 edge cases3") {
    println(Task1.twoDigitSpelled("threeighthree"))
  }

  test("syntax") {
    val l = List("a", "b")
    println(l.indexOf("c"))
    println("afgagadga".substring(1))
    val p = "one.*".r
    val all = p.findAllMatchIn("oneoneoneonetwoone")

    for (a <- all) {
      println(a.group(0))

    }
  }
}
