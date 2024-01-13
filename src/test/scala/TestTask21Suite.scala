package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask21Suite extends AnyFunSuite {

  test("task21_1 test file") {
    println(Task21.calcFile1(Source.fromResource("input_test_21.txt"), 6))
  } // 16

  test("task21_1 all file") {
    println(Task21.calcFile1(Source.fromResource("input_all_21.txt"), 1000))
  } // 3699

  test("task21_2 test file") {
    println(Task21.calcFile2(Source.fromResource("input_test_21.txt"), 5000))
  } // 16733044

  test("task21_2 all file") {
    println(Task21.calcFile2(Source.fromResource("input_all_21.txt"), 1000))
  } //
}
