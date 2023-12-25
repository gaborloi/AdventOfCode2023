package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask11Suite extends AnyFunSuite {

  test("task11_1 test file") {
    println(Task11.calcFile1(Source.fromResource("input_test_11.txt"), 1))
  } // 374

  test("task11_1 all file") {
    println(Task11.calcFile1(Source.fromResource("input_all_11.txt"), 1))
  } // 9550717

  test("task11_2 test file") {
    println(Task11.calcFile2(Source.fromResource("input_test_11.txt"), 99))
  } // 8410

  test("task11_2 all file") {
    println(Task11.calcFile2(Source.fromResource("input_all_11.txt"), 999999))
  } //
}
