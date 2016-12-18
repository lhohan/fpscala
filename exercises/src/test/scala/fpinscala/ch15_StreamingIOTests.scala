package fpinscala

import org.scalatest.FunSuite

import fpinscala.streamingio.SimpleStreamTransducers._

class ch15_StreamingIOTests extends FunSuite {
  test("15.1: take") {
    val p: Process[Int, Int] = Process.take(3)
    assert(List(1, 2, 3) == p(Stream(1, 2, 3, 4, 5)).toList)
    assert(List(1) == p(Stream(1)).toList)
    assert(List() == p(Stream()).toList)
  }

  test("15.1: drop") {
    val p: Process[Int, Int] = Process.drop(3)
    assert(List(4, 5) == p(Stream(1, 2, 3, 4, 5)).toList)
    assert(List() == p(Stream(1)).toList)
    assert(List() == p(Stream()).toList)
  }

  test("15.1: takeWhile") {
    val p: Process[Int, Int] = Process.takeWhile(_ < 3)
    assert(List(1, 2) == p(Stream(1, 2, 3, 4, 5)).toList)
    assert(List(1, 1, 0) == p(Stream(1, 1, 0)).toList)
    assert(List() == p(Stream()).toList)
    assert(List() == p(Stream(5, 1, 2, 3)).toList)
  }

  test("15.1: dropWhile") {
    val p: Process[Int, Int] = Process.dropWhile(_ < 3)
    assert(List(3, 4, 5) == p(Stream(1, 2, 3, 4, 5)).toList)
    assert(List() == p(Stream(1, 1, 0)).toList)
    assert(List() == p(Stream()).toList)
    assert(List(5, 1, 2, 3) == p(Stream(5, 1, 2, 3)).toList)
  }

  test("15.2: count") {
    val p: Process[String, Int] = Process.count
    assert(List(0, 1, 2, 3) == p(Stream("a", "b", "c")).toList)
    assert(List(0, 1) == p(Stream("a")).toList)
    assert(List(0) == p(Stream.empty[String]).toList)
  }

  test("15.3: mean") {
    val p: Process[Double, Double] = Process.mean
    assert(List(1.0, 1.0, 1.0) == p(Stream(1.0, 1.0, 1.0)).toList)
    assert(List(1.0, 2.0, 3.0) == p(Stream(1.0, 3.0, 5.0)).toList)
    assert(List(5.0) == p(Stream(5.0)).toList)
    assert(List() == p(Stream()).toList)
  }

  test("15.4: count3") {
    val p: Process[String, Int] = Process.count3
    assert(List(1, 2, 3) == p(Stream("a", "b", "c")).toList)
    assert(List(1) == p(Stream("a")).toList)
    assert(List() == p(Stream.empty[String]).toList)
  }

  test("15.4: sum2") {
    val p: Process[Double, Double] = Process.sum2
    assert(List(1.0, 2.0, 3.0) == p(Stream(1.0, 1.0, 1.0)).toList)
    assert(List(1.0, 4.0, 9.0) == p(Stream(1.0, 3.0, 5.0)).toList)
    assert(List(5.0) == p(Stream(5.0)).toList)
    assert(List() == p(Stream()).toList)
  }
}
