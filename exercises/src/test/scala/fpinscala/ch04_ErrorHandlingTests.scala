package fpinscala

import org.scalatest.FunSuite

/**
 * Created by hans on 25/10/14.
 */
class ch04_ErrorHandlingTests extends FunSuite {

  test("map") {
    import fpinscala.errorhandling._
    def f(x: Int) = x + 1

    assertResult(None, "none")(None.map(f))
    assertResult(Some(6), "some")(Some(5).map(f))
  }

  test("getOrElse") {
    import fpinscala.errorhandling._

    assertResult("x", "none")(None.getOrElse("x"))
    assertResult(5, "some")(Some(5).getOrElse(42))
  }

  test("flatMap") {
    import fpinscala.errorhandling._
    def f(x: Int) = Some(x + 1)

    assertResult(None, "none")(None.flatMap(f))
    assertResult(Some(6), "some")(Some(5).flatMap(f))
  }

  test("orElse") {
    import fpinscala.errorhandling._

    assertResult(Some("default option value"), "none")(None.orElse(Some("default option value")))
    assertResult(Some(5), "some")(Some(5).orElse(throw new IllegalArgumentException("I should not be evaluated")))
  }

  test("filter") {
    import fpinscala.errorhandling._

    assertResult(None, "none filter false")(None.filter(_ => false))
    assertResult(None, "none filter true")(None.filter(_ => true))
    assertResult(None, "some filter false")(Some(5).filter(_ % 2 == 0))
    assertResult(Some(6), "some filter true")(Some(6).filter(_ % 2 == 0))
  }
}
