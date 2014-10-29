package fpinscala

import org.scalatest.FunSuite

/**
 * Created by hans on 29/10/14.
 */
  class ch05_LazinessTests extends FunSuite {

    test("toList") {
      import fpinscala.laziness._

      assertResult(List(1,2,3), "toList")(Stream(1,2,3).toList)
    }

}
