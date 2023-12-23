package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask9Suite extends AnyFunSuite {

  test("task9_1 test file") {
    println(Task9.calcFile1(Source.fromResource("input_test_9.txt")))
  } // 114

  test("task9_1 all file") {
    println(Task9.calcFile1(Source.fromResource("input_all_9.txt")))
  } // 1930746032

  test("task9_2 test file") {
    println(Task9.calcFile2(Source.fromResource("input_test_9.txt")))
  } // 2

  test("task9_2 all file") {
    println(Task9.calcFile2(Source.fromResource("input_all_9.txt")))
  } // 1154
}
