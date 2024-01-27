package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask23Suite extends AnyFunSuite {

  test("task23_1 test file") {
    assert(Task23.calcFile1(Source.fromResource("input_test_23.txt")) == 94)
  }

  test("task23_1 all file") {
    assert(Task23.calcFile1(Source.fromResource("input_all_23.txt")) == 2230)
  }

  test("task23_2 test file") {
    assert(Task23.calcFile2(Source.fromResource("input_test_23.txt")) == 154)
  }

  test("task23_2 all file") {
    assert(Task23.calcFile2(Source.fromResource("input_all_23.txt")) == 71002)
  }
}
