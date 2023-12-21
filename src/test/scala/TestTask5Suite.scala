package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask5Suite extends AnyFunSuite {

  test("task5_1 test file") {
    println(Task5.parseFile1(Source.fromResource("input_test_5.txt")))
  } // 35

  test("task5_1 all file") {
    println(Task5.parseFile1(Source.fromResource("input_all_5.txt")))
  } // 51 580 674

  test("task5_2 test file") {
    println(Task5.parseFile2(Source.fromResource("input_test_5.txt")))
  } // 46

  test("task5_2 all file") {
    println(Task5.parseFile2(Source.fromResource("input_all_5.txt")))
  } // 99 751 240
}
