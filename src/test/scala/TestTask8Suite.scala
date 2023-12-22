package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask8Suite extends AnyFunSuite {

  test("task8_1a test file") {
    println(Task8.calcFile1(Source.fromResource("input_test_8_1.txt")))
  } // 2

  test("task8_1b test file") {
    println(Task8.calcFile1(Source.fromResource("input_test_8_2.txt")))
  } // 6

  test("task8_1 all file") {
    println(Task8.calcFile1(Source.fromResource("input_all_8.txt")))
  } // 15871

  test("task8_2 test file") {
    println(Task8.calcFile2(Source.fromResource("input_test_8_3.txt")))
  } // 6

  test("task8_2 all file") {
    println(Task8.calcFile2(Source.fromResource("input_all_8.txt")))
  } //
}
