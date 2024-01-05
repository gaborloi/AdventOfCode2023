package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask18Suite extends AnyFunSuite {

  test("task18_1 test file") {
    println(Task18.calcFile1(Source.fromResource("input_test_18.txt")))
  } // 62

  test("task18_1 all file") {
    println(Task18.calcFile1(Source.fromResource("input_all_18.txt")))
  } // 40714
  test("task18_2 test file") {
    println(Task18.calcFile2(Source.fromResource("input_test_18.txt")))
  } // 952408144115

  test("task18_2 all file") {
    println(Task18.calcFile2(Source.fromResource("input_all_18.txt")))
  } //

  test("tryout") {
    val x = Range(0, 3)
    val y = Range.inclusive(0, 3)
    println(x.max,y.max)
  }
}
