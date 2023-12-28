package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask13Suite extends AnyFunSuite {

  test("task13_1 test file") {
    println(Task13.calcFile1(Source.fromResource("input_test_13.txt")))
  } // 405

  test("task13_1 all file") {
    println(Task13.calcFile1(Source.fromResource("input_all_13.txt")))
  } // 26957

  test("task13_2 test file") {
    println(Task13.calcFile2(Source.fromResource("input_test_13.txt")))
  } // 400

  test("task13_2 debug file") {
    println(Task13.calcFile2(Source.fromResource("input_debug_13.txt")))
  } // 1300

  test("task13_2 all file") {
    println(Task13.calcFile2(Source.fromResource("input_all_13.txt")))
  } // 42695
}
