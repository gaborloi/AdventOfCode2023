package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask16Suite extends AnyFunSuite {

  test("task16_1 test file") {
    println(Task16.calcFile1(Source.fromResource("input_test_16.txt")))
  } // 46

  test("task16_1 all file") {
    println(Task16.calcFile1(Source.fromResource("input_all_16.txt")))
  } // 6816

  test("task16_2 test file") {
    println(Task16.calcFile2(Source.fromResource("input_test_16.txt")))
  } // 51

  test("task16_2 all file") {
    println(Task16.calcFile2(Source.fromResource("input_all_16.txt")))
  } // 8163
}
