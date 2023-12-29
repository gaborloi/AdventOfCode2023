package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask14Suite extends AnyFunSuite {

  test("task14_1 test file") {
    println(Task14.calcFile1(Source.fromResource("input_test_14.txt")))
  } // 136

  test("task14_1 all file") {
    println(Task14.calcFile1(Source.fromResource("input_all_14.txt")))
  } // 109596

  test("task14_2 test file") {
    println(Task14.calcFile2(Source.fromResource("input_test_14.txt")))
  } // 64

  test("task14_2 all file") {
    println(Task14.calcFile2(Source.fromResource("input_all_14.txt")))
  } //
}
