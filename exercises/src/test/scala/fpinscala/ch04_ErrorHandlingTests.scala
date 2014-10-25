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
}
