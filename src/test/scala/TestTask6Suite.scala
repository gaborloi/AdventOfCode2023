package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask6Suite extends AnyFunSuite {

  test("task6_1 test file") {
    println(Task6.parseFile1(Source.fromResource("input_test_6.txt")))
  } // 288

  test("task6_1 all file") {
    println(Task6.parseFile1(Source.fromResource("input_all_6.txt")))
  } // 861300

  test("task6_2 test file") {
    println(Task6.parseFile2(Source.fromResource("input_test_6.txt")))
  } // 71503

  test("task6_2 all file") {
    println(Task6.parseFile2(Source.fromResource("input_all_6.txt")))
  } // 28 101 347
}
