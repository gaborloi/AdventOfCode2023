package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask3Suite extends AnyFunSuite {

  test("task3_1 test file") {
    println(Task3.parseFile1(Source.fromResource("input_test_3.txt")))
  }

  test("task3_1 all file") {
    println(Task3.parseFile1(Source.fromResource("input_all_3.txt")))
  }

  test("task3_2 test file") {
    println(Task3.parseFile2(Source.fromResource("input_test_3.txt")))
  }

  test("task3_2 all file") {
    println(Task3.parseFile2(Source.fromResource("input_all_3.txt")))
  }
}
