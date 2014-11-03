package fpinscala

import org.scalatest.FunSuite

/**
 * Created by hans on 29/10/14.
 */
class ch05_LazinessTests extends FunSuite {

  test("toList") {
    import fpinscala.laziness._

    assertResult(List(1, 2, 3), "toList")(Stream(1, 2, 3).toList)
    assertResult(List(), "empty")(Stream().toList)
  }

  test("take") {
    import fpinscala.laziness._

    assertResult(List(1, 2), "base")(Stream(1, 2, 3).take(2).toList)
    assertResult(List(), "empty")(Stream().take(0).toList)
  }

  test("drop") {
    import fpinscala.laziness._

    assertResult(List(3), "base")(Stream(1, 2, 3).drop(2).toList)
    assertResult(List(1, 2, 3), "drop none")(Stream(1, 2, 3).drop(0).toList)
    assertResult(List(), "drop all exactly")(Stream(1, 2, 3).drop(3).toList)
    assertResult(List(), "empty")(Stream().drop(0).toList)
  }

  test("drop - tailrec") {
    import fpinscala.laziness._

    assertResult(List(3), "base")(Stream(1, 2, 3).drop2(2).toList)
    assertResult(List(1, 2, 3), "drop none")(Stream(1, 2, 3).drop2(0).toList)
    assertResult(List(), "drop all exactly")(Stream(1, 2, 3).drop2(3).toList)
    assertResult(List(), "empty")(Stream().drop2(0).toList)
  }

  test("takeWhile") {
    import fpinscala.laziness._

    assertResult(List(1, 2), "base")(Stream(1, 2, 3).takeWhile(_ < 3).toList)
    assertResult(List(1, 2, 3), "base")(Stream(1, 2, 3).takeWhile(_ < 5).toList)
    assertResult(List(), "empty")(Stream[Int]().takeWhile(_ < 3).toList)
  }

  test("forAll") {
    import fpinscala.laziness._

    assertResult(true, "base")(Stream(1, 2, 3).forAll(_ < 4))
    assertResult(false, "base - false 1")(Stream(1, 2, 3).forAll(_ <= 1))
    assertResult(false, "base - false 2")(Stream(1, 2, 3).forAll(_ < 1))
    assertResult(true, "empty")(Stream[Int]().forAll(_ < 4))
  }

  test("takeWhileViaFoldRight") {
    import fpinscala.laziness._

    assertResult(List(1, 2), "base")(Stream(1, 2, 3).takeWhileViaFoldRight(_ < 3).toList)
    assertResult(List(1, 2, 3), "base")(Stream(1, 2, 3).takeWhileViaFoldRight(_ < 5).toList)
    assertResult(List(), "empty")(Stream[Int]().takeWhileViaFoldRight(_ < 3).toList)
  }

  test("headOption - ViaFoldRight") {
    import fpinscala.laziness._

    assertResult(Some(1), "head")(Stream(1, 2, 3).headOption)
    assertResult(None, "no head")(Stream().headOption)
  }

  test("map") {
    import fpinscala.laziness._

    assertResult(List(2, 4, 6), "base")(Stream(1, 2, 3).map(_ * 2).toList)
    assertResult(List(), "empty")(Stream[Int]().map(_ * 2).toList)
  }

  test("filter") {
    import fpinscala.laziness._

    assertResult(List(1, 3), "base")(Stream(1, 2, 3).filter(_ % 2 != 0).toList)
    assertResult(List(), "empty")(Stream[Int]().filter(_ % 2 != 0).toList)
  }

  test("append") {
    import fpinscala.laziness._

    assertResult(List(1, 2, 3, 4, 5), "base")(Stream(1, 2, 3).append(Stream(4, 5)).toList)
    assertResult(List(1, 2, 3), "arg empty")(Stream(1, 2, 3).append(Stream()).toList)
    assertResult(List(4, 5), "empty")(Stream().append(Stream(4, 5)).toList)
    assertResult(List(), "both empty")(Stream().append(Stream()).toList)
  }

  test("flatMap") {
    import fpinscala.laziness._

    assertResult(List("1", "1", "2", "2", "3", "3"), "base")(Stream(1, 2, 3).flatMap(i => Stream(i.toString, i.toString)).toList)
  }

  test("constant infinite stream") {
    import fpinscala.laziness.Stream._

    assertResult(List("a", "a", "a", "a", "a"), "constant a")(constant("a").take(5).toList)
  }

  test("from stream") {
    import fpinscala.laziness.Stream._

    assertResult(List(5, 6, 7), "from 5")(from(5).take(3).toList)
  }

  test("fibs") {
    import fpinscala.laziness.Stream._
    assertResult(List(0, 1, 1, 2, 3, 5, 8), "fibs 7")(fibs.take(7).toList)
  }

}
