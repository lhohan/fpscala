package fpinscala

import java.util.concurrent.Executors

import fpinscala.monads.Monad._
import org.scalatest.FunSuite
import fpinscala.parallelism.Nonblocking.Par

class ch11_MonadTests extends FunSuite {

  test("ex 11.1 - parMonad") {
    import parMonad._
    val p1 = unit(4)
    val p2 = flatMap(p1)(i => unit(i + 1))
    val es = Executors.newFixedThreadPool(2)
    val result = Par.run(es)(p2)
    assert(5 == result)
  }

  test("ex 11.1 - list") {
    import listMonad._
    val m1 = unit(4)
    val result = flatMap(m1)(i => List.fill(i)(i))
    assert(List(4, 4, 4, 4) == result)
  }

  test("ex 11.1 - stream") {
    import streamMonad._
    val m1 = unit(4)
    val result = flatMap(m1)(i => Stream.continually(i))
    assert(List(4, 4, 4, 4) == result.take(4).toList)
  }

}
