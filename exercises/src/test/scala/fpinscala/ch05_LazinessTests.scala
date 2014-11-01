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

}
