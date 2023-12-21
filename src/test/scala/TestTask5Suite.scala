package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask4Suite extends AnyFunSuite {

  test("task4_1 test file") {
    println(Task4.parseFile1(Source.fromResource("input_test_4.txt")))
  }

  test("task4_1 all file") {
    println(Task4.parseFile1(Source.fromResource("input_all_4.txt")))
  }

  test("task4_2 test file") {
    println(Task4.parseFile2(Source.fromResource("input_test_4.txt")))
  }

  test("task4_2 all file") {
    println(Task4.parseFile2(Source.fromResource("input_all_4.txt")))
  }
}
