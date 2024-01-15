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
    println(Task21.calcFile2(Source.fromResource("input_all_21.txt"), 174))//26501365))
  } // 613391963786150 too high 64-> 3699, 131 -> 15199, 200 -> 35361, 500 -> 219193, 600 -> 316048, 700 -> 428419 875527, 877767
    // 613391294982479  429933

  test("task21_2 all file slow") {
    println(Task21.calcFile3(Source.fromResource("input_all_21.txt"), 175))//26501365))
  } //131 -> 15199, 200 -> 35361, 500 -> 219193, 700 -> 428424, 875534, 877785

  test("try") {
    (1 to 710) foreach { i =>
      val faster = Task21.calcFile2(Source.fromResource("input_all_21.txt"), i)
      val slower = Task21.calcFile3(Source.fromResource("input_all_21.txt"), i)
      if (faster != slower) println(i, faster, slower)
    }
  }
}
