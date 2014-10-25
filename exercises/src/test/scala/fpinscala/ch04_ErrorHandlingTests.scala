package fpinscala

import org.scalatest.FunSuite

/**
 * Created by hans on 25/10/14.
 */
class ch04_ErrorHandlingTests extends FunSuite {

  test("map"){
    import fpinscala.errorhandling._
    def f(x:Int) = x+1

    assertResult(None, "none")(None.map(f))
    assertResult(Some(6), "some")(Some(5).map(f))
  }

  test("getOrElse"){
    import fpinscala.errorhandling._

    assertResult("x", "none")(None.getOrElse("x"))
    assertResult(5, "some")(Some(5).getOrElse(42))
  }

  test("flatMap"){
    import fpinscala.errorhandling._
    def f(x:Int) = Some(x+1)

    assertResult(None, "none")(None.flatMap(f))
    assertResult(Some(6), "some")(Some(5).flatMap(f))
  }

  test("orElse"){
    import fpinscala.errorhandling._

    assertResult(Some("default option value"), "none")(None.orElse(Some("default option value")))
    assertResult(Some(5), "some")(Some(5).orElse(throw new IllegalArgumentException("I should not be evaluated")))
  }
}
