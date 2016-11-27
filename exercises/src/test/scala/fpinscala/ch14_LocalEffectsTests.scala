package fpinscala

import fpinscala.localeffects.{RunnableST, ST, STArray}
import org.scalatest.FunSuite

/**
  * Created by hans on 27/11/16.
  */
class ch14_LocalEffectsTests extends FunSuite {

  test("14.0 - some tests with STArray") {

    val p = new RunnableST[(String, List[String])] {
      override def apply[S]: ST[S, (String, List[String])] =
        for {
          arr  <- STArray(5, "a")
          _    <- arr.write(2, "b")
          x    <- arr.freeze
          head <- arr.read(0)
        } yield (head, x)
    }
    val (head, list) = ST.runST(p)
    assert(List("a", "a", "b", "a", "a") === list)
    assert("a" === head)

  }

  test("14.1 - fill using Map") {
    type ResultType = List[String]

    val p = new RunnableST[ResultType] {
      override def apply[S]: ST[S, ResultType] =
        for {
          arr <- STArray(5, "a")
          _   <- arr.fill(Map(0 -> "A", 4 -> "E"))
          x   <- arr.freeze
        } yield x
    }
    val list = ST.runST(p)
    assert(List("A", "a", "a", "a", "E") === list)

  }

}
