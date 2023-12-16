package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask2Suite extends AnyFunSuite {

  test("task2_1 test file") {
    println(Task2.parseFile(Source.fromResource("input_test_2_1.txt")))
  }

  test("task2_1 all file") {
    println(Task2.parseFile(Source.fromResource("input_all_2_1.txt")))
  }

  test("task2_2 test file") {
    println(Task2.parseFile2(Source.fromResource("input_test_2_1.txt")))
  }

  test("task2_2 all file") {
    println(Task2.parseFile2(Source.fromResource("input_all_2_1.txt")))
  }
}
