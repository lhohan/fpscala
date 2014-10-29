package fpinscala

import org.scalatest.FunSuite

/**
 * Created by hans on 29/10/14.
 */
  class ch05_LazinessTests extends FunSuite {

    test("toList") {
      import fpinscala.laziness._

      assertResult(List(1,2,3), "toList")(Stream(1,2,3).toList)
      assertResult(List(), "empty")(Stream().toList)
    }

    test("take") {
      import fpinscala.laziness._

      assertResult(List(1,2), "base")(Stream(1,2,3).take(2).toList)
      assertResult(List(), "empty")(Stream().take(0).toList)
    }

}
