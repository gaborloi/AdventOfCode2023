package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask25Suite extends AnyFunSuite {

  test("task25_1 test file") {
    assert(Task25.calcFile1(Source.fromResource("input_test_25.txt")) == 54)
  }

  test("task25_1 all file") {
    assert(Task25.calcFile1(Source.fromResource("input_all_25.txt")) == 16502)
  }

  test("task25_2 test file") {
    assert(Task25.calcFile2(Source.fromResource("input_test_25.txt")) == 47)
  }

  test("task25_2 all file") {
    assert(Task25.calcFile2(Source.fromResource("input_all_25.txt")) == 6)
  }
}
