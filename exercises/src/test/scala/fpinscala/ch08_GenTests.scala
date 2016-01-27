package fpinscala

import fpinscala.testing.{ Prop, Gen }
import org.scalatest.FunSuite

/**
 * Created by hans on 27/01/16.
 */
class ch08_GenTests extends FunSuite {

  test("ex 8.3") {
    val p1 = new Prop {
      override def check: Boolean = true
    }
    val p2 = new Prop {
      override def check: Boolean = true
    }
    val p3 = p1 && p2
    assert(p3.check == true)
  }
}
