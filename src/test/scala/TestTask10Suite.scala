package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask10Suite extends AnyFunSuite {

  test("task10_1 test file") {
    println(Task10.calcFile1(Source.fromResource("input_test_10_1.txt")))
  } // 4

  test("task10_2 test file") {
    println(Task10.calcFile1(Source.fromResource("input_test_10_2.txt")))
  } // 8

  test("task10 all file") {
    println(Task10.calcFile1(Source.fromResource("input_all_10.txt")))
  } // 6870

  test("task9_2 test file") {
    println(Task9.calcFile2(Source.fromResource("input_test_9.txt")))
  } // 2

  test("task9_2 all file") {
    println(Task9.calcFile2(Source.fromResource("input_all_9.txt")))
  } // 1154
}
