package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask11Suite extends AnyFunSuite {

  test("task11 test file") {
    println(Task11.calcFile1(Source.fromResource("input_test_11.txt")))
  } // 374

  test("task11 all file") {
    println(Task11.calcFile1(Source.fromResource("input_all_11.txt")))
  } //

  test("task10b_1 test file") {
    println(Task10.calcFile2(Source.fromResource("input_test_10_1.txt")))
  } // 1

  test("task10b_2 test file") {
    println(Task10.calcFile2(Source.fromResource("input_test_10_2.txt")))
  } // 1

  test("task10b_3 test file") {
    println(Task10.calcFile2(Source.fromResource("input_test_10_3.txt")))
  } // 4

  test("task10b_4 test file") {
    println(Task10.calcFile2(Source.fromResource("input_test_10_4.txt")))
  } // 4

  test("task10_2 all file") {
    println(Task10.calcFile2(Source.fromResource("input_all_10.txt")))
  } //
}
