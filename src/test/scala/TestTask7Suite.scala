package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask7Suite extends AnyFunSuite {

  test("task7_1 test file") {
    println(Task7.calcFile1(Source.fromResource("input_test_7.txt")))
  } //6440

  test("task7_1 all file") {
    println(Task7.calcFile1(Source.fromResource("input_all_7.txt")))
  } // 248 836 197

  test("task7_2 test file") {
    println(Task7.calcFile2(Source.fromResource("input_test_7.txt")))
  } // 5905

  test("task7_2 all file") {
    println(Task7.calcFile2(Source.fromResource("input_all_7.txt")))
  } // 251 195 607
}
