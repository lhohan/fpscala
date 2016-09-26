package fpinscala

import org.scalatest.FunSuite
import fpinscala.applicative.Applicative._
import fpinscala.applicative.Monad._

class ch12_ApplicativeTraversableFunctorTests extends FunSuite {

  test("12.4") {

    val sequenced = streamApplicative.sequence(List(Stream(1, 2, 3), Stream(4, 5, 6, 7)))

    assert(List(List(1, 4), List(2, 5), List(3, 6)) == sequenced)
    // so:
    /*
    1 2 3
    4 5 6
    becomes
    1 4
    2 5
    3 6
    so sequencing means 'transposition of a matrix'
     */

  }

  test("12.5") {
    assert(Right(6) == eitherMonad.map(Right(5))(_ + 1))
    assert(Left(5) == eitherMonad.map(Left[Int, Int](5))(_ + 1))
  }
}
