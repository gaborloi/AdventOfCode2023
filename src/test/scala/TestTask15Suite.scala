package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask15Suite extends AnyFunSuite {

  test("task15_1 test file") {
    println(Task15.calcFile1(Source.fromResource("input_test_15.txt")))
  } // 1320

  test("task15_1 all file") {
    println(Task15.calcFile1(Source.fromResource("input_all_15.txt")))
  } // 515974

  test("task15_2 test file") {
    println(Task15.calcFile2(Source.fromResource("input_test_15.txt")))
  } // 145

  test("task15_2 all file") {
    println(Task15.calcFile2(Source.fromResource("input_all_15.txt")))
  } // 96105 too low
}
