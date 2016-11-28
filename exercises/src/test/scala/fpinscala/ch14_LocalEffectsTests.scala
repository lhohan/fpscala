package fpinscala

import fpinscala.localeffects._
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

  test("14.0 - test swap") {
    type ResultType = List[String]

    val p = new RunnableST[ResultType] {
      override def apply[S]: ST[S, ResultType] =
        for {
          arr <- STArray(5, "a")
          _   <- arr.fill(Map(0 -> "A", 1 -> "B", 2 -> "C", 3 -> "D", 4 -> "E"))
          _   <- arr.swap(0, 4)
          x   <- arr.freeze
        } yield x
    }
    val list = ST.runST(p)
    assert(List("E", "B", "C", "D", "A") === list)
  }

  test("14.2 - quicksort") {
    assert(List(1, 2, 3, 4, 5) === Immutable.quicksort(List(1, 2, 3, 4, 5)))
    assert(List(1, 2, 3, 4, 5) === Immutable.quicksort(List(1, 4, 5, 2, 3)))
    assert(List(1, 2, 3, 4, 5) === Immutable.quicksort(List(5, 4, 3, 2, 1)))
  }

  test("14.3 - STHashMap") {
    type ResultType = (Map[String, Int], Int, Set[String])

    val p = new RunnableST[ResultType] {
      override def apply[S]: ST[S, ResultType] =
        for {
          m    <- STHashMap(("a", 1), ("b", 2))
          _    <- m.put("c", 3)
          cVal <- m.get("c")
          keys <- m.keys
          x    <- m.freeze
        } yield (x, cVal, keys)
    }
    val (result, getResult, keysResult) = ST.runST(p)
    assert(Map("a" -> 1, "b" -> 2, "c" -> 3) === result)
    assert(3 === getResult)
    assert(Set("a", "b", "c") === keysResult)
  }

}
