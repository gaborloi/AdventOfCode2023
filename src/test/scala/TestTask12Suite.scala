package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask12Suite extends AnyFunSuite {

  test("task12_1 test file") {
    println(Task12.calcFile1(Source.fromResource("input_test_12.txt")))
  } // 21

  test("task12_1 debug file") {
    println(Task12.calcFile1(Source.fromResource("input_debug_12.txt")))
  } // 43

  test("task12_1 all file") {
    println(Task12.calcFile1(Source.fromResource("input_all_12.txt")))
  } // 7195

  test("task12_2 test file") {
    println(Task12.calcFile2(Source.fromResource("input_test_12.txt")))
  } //

  test("task12_2 all file") {
    println(Task12.calcFile2(Source.fromResource("input_all_12.txt")))
  } //
}
