package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask19Suite extends AnyFunSuite {

  test("task19_1 test file") {
    println(Task19.calcFile1(Source.fromResource("input_test_19.txt")))
  } // 19114

  test("task19_1 all file") {
    println(Task19.calcFile1(Source.fromResource("input_all_19.txt")))
  } // 476889

  test("task19_2 test file") {
    println(Task19.calcFile2(Source.fromResource("input_test_19.txt")))
  } // 167409079868000

  test("task19_2 all file") {
    println(Task19.calcFile2(Source.fromResource("input_all_19.txt")))
  } // 132380153677887
}
