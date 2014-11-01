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

}
