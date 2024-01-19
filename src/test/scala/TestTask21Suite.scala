package org.practice.advent

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask21Suite extends AnyFunSuite {

  test("task21_1 test file") {
    println(Task21.calcFile1(Source.fromResource("input_test_21.txt"), 6))
  } // 16

  test("task21_1 all file") {
    println(Task21.calcFile1(Source.fromResource("input_all_21.txt"), 64))
  } // 3699

  test("task21_2 test file") {
    println(Task21.calcFile2(Source.fromResource("input_test_21.txt"), 5000))
  } // 16733044

  test("task21_2 all file") {
    println(Task21.calcFile2(Source.fromResource("input_all_21.txt"), 26501365))
  } // 613391294577878

  test("task21_2 all file slow") {
    println(Task21.calcFile3(Source.fromResource("input_all_21.txt"), 263))
  } //131 -> 15199, 200 -> 35361, 500 -> 219193, 700 -> 428424, 1000 -> 875534, 877785, 2000 ->

  test("try") {
    (260 to 800) foreach { i =>
      val faster = Task21.calcFile2(Source.fromResource("input_all_21.txt"), i)
      val slower = Task21.calcFile3(Source.fromResource("input_all_21.txt"), i)
      if (faster != slower) println(i, faster, slower)
    }
  }
}
