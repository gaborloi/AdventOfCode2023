package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask20Suite extends AnyFunSuite {

  test("task20_1 test file") {
    println(Task20.calcFile1(Source.fromResource("input_test_20.txt")))
  } // 32000000

  test("task20_1 all file") {
    println(Task20.calcFile1(Source.fromResource("input_all_20.txt")))
  } // 899848294

  test("task20_2 all file") {
    println(Task20.calcFile2(Source.fromResource("input_all_20.txt")))
  } // 697443173 too low
}
