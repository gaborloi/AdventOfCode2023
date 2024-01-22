package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask22Suite extends AnyFunSuite {

  test("task22_1 test file") {
    assert(Task22.calcFile1(Source.fromResource("input_test_22.txt")) == 5)
  }

  test("task22_1 all file") {
    assert(Task22.calcFile1(Source.fromResource("input_all_22.txt")) == 505)
  }

  test("task22_2 test file") {
    println(Task22.calcFile2(Source.fromResource("input_test_22.txt")))
  } //

  test("task22_2 all file") {
    println(Task22.calcFile2(Source.fromResource("input_all_22.txt")))
  } //
}
