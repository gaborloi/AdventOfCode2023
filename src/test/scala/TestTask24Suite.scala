package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask24Suite extends AnyFunSuite {

  test("task24_1 test file") {
    assert(Task24.calcFile1(Source.fromResource("input_test_24.txt"), 7, 27) == 2)
  }

  test("task24_1 all file") {
    assert(Task24.calcFile1(Source.fromResource("input_all_24.txt"), 200000000000000L, 400000000000000L) == 16502)
  }

  test("task24_2 test file") {
    assert(Task24.calcFile2(Source.fromResource("input_test_24.txt")) == 47)
  }

  test("task24_2 all file") {
    assert(Task24.calcFile2(Source.fromResource("input_all_24.txt")) == 673641951253289L)
  }
}
