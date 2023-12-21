package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask6Suite extends AnyFunSuite {

  test("task6_1 test file") {
    println(Task6.parseFile1(Source.fromResource("input_test_6.txt")))
  } // 3150

//  test("task5_1 all file") {
//    println(Task5.parseFile1(Source.fromResource("input_all_5.txt")))
//  } // 51 580 674
//
//  test("task5_2 test file") {
//    println(Task5.parseFile2(Source.fromResource("input_test_5.txt")))
//  } // 46
//
//  test("task5_2 all file") {
//    println(Task5.parseFile2(Source.fromResource("input_all_5.txt")))
//  } // 99 751 240
}
