package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask17Suite extends AnyFunSuite {

  test("task17_1 test file") {
    println(Task17.calcFile1(Source.fromResource("input_test_17.txt")))
  } // 102

  test("task17_1 all file") {
    println(Task17.calcFile1(Source.fromResource("input_all_17.txt")))
  } //

  test("task17_2 test file") {
    println(Task17.calcFile2(Source.fromResource("input_test_17.txt")))
  } // 94

  test("task17_2 all file") {
    println(Task17.calcFile2(Source.fromResource("input_all_17.txt")))
  } //
}
