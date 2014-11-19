package fpinscala

import org.scalatest.FunSuite

/**
 * Created by hans on 29/10/14.
 */
class ch05_LazinessTests extends FunSuite {

  test("toList") {
    import fpinscala.laziness._

    assertResult(List(1, 2, 3), "toList")(Stream(1, 2, 3).toList)
    assertResult(List(), "empty")(Stream().toList)
  }

  test("take") {
    import fpinscala.laziness._

    assertResult(List(1, 2), "base")(Stream(1, 2, 3).take(2).toList)
    assertResult(List(), "empty")(Stream().take(0).toList)
  }

  test("drop") {
    import fpinscala.laziness._

    assertResult(List(3), "base")(Stream(1, 2, 3).drop(2).toList)
    assertResult(List(1, 2, 3), "drop none")(Stream(1, 2, 3).drop(0).toList)
    assertResult(List(), "drop all exactly")(Stream(1, 2, 3).drop(3).toList)
    assertResult(List(), "empty")(Stream().drop(0).toList)
  }

  test("drop - tailrec") {
    import fpinscala.laziness._

    assertResult(List(3), "base")(Stream(1, 2, 3).drop2(2).toList)
    assertResult(List(1, 2, 3), "drop none")(Stream(1, 2, 3).drop2(0).toList)
    assertResult(List(), "drop all exactly")(Stream(1, 2, 3).drop2(3).toList)
    assertResult(List(), "empty")(Stream().drop2(0).toList)
  }

  test("takeWhile") {
    import fpinscala.laziness._

    assertResult(List(1, 2), "base")(Stream(1, 2, 3).takeWhile(_ < 3).toList)
    assertResult(List(1, 2, 3), "base")(Stream(1, 2, 3).takeWhile(_ < 5).toList)
    assertResult(List(), "empty")(Stream[Int]().takeWhile(_ < 3).toList)
  }

  test("forAll") {
    import fpinscala.laziness._

    assertResult(true, "base")(Stream(1, 2, 3).forAll(_ < 4))
    assertResult(false, "base - false 1")(Stream(1, 2, 3).forAll(_ <= 1))
    assertResult(false, "base - false 2")(Stream(1, 2, 3).forAll(_ < 1))
    assertResult(true, "empty")(Stream[Int]().forAll(_ < 4))
  }

  test("takeWhileViaFoldRight") {
    import fpinscala.laziness._

    assertResult(List(1, 2), "base")(Stream(1, 2, 3).takeWhileViaFoldRight(_ < 3).toList)
    assertResult(List(1, 2, 3), "base")(Stream(1, 2, 3).takeWhileViaFoldRight(_ < 5).toList)
    assertResult(List(), "empty")(Stream[Int]().takeWhileViaFoldRight(_ < 3).toList)
  }

  test("headOption - ViaFoldRight") {
    import fpinscala.laziness._

    assertResult(Some(1), "head")(Stream(1, 2, 3).headOption)
    assertResult(None, "no head")(Stream().headOption)
  }

  test("map") {
    import fpinscala.laziness._

    assertResult(List(2, 4, 6), "base")(Stream(1, 2, 3).map(_ * 2).toList)
    assertResult(List(), "empty")(Stream[Int]().map(_ * 2).toList)
  }

  test("filter") {
    import fpinscala.laziness._

    assertResult(List(1, 3), "base")(Stream(1, 2, 3).filter(_ % 2 != 0).toList)
    assertResult(List(), "empty")(Stream[Int]().filter(_ % 2 != 0).toList)
  }

  test("append") {
    import fpinscala.laziness._

    assertResult(List(1, 2, 3, 4, 5), "base")(Stream(1, 2, 3).append(Stream(4, 5)).toList)
    assertResult(List(1, 2, 3), "arg empty")(Stream(1, 2, 3).append(Stream()).toList)
    assertResult(List(4, 5), "empty")(Stream().append(Stream(4, 5)).toList)
    assertResult(List(), "both empty")(Stream().append(Stream()).toList)
  }

  test("flatMap") {
    import fpinscala.laziness._
    assertResult(List("1", "1", "2", "2", "3", "3"), "base")(Stream(1, 2, 3).flatMap(i => Stream(i.toString, i.toString)).toList)
  }

  test("constant infinite stream") {
    import fpinscala.laziness.Stream._
    assertResult(List("a", "a", "a", "a", "a"), "constant a")(constant("a").take(5).toList)
  }

  test("from stream") {
    import fpinscala.laziness.Stream._
    assertResult(List(5, 6, 7), "from 5")(from(5).take(3).toList)
  }

  test("fibs") {
    import fpinscala.laziness.Stream._
    assertResult(List(0, 1, 1, 2, 3, 5, 8), "fibs 7")(fibs.take(7).toList)
  }

  test("unfold") {
    import fpinscala.laziness.Stream._
    assertResult(List(5, 6, 7), "unfold 5")(unfold(5)(x => Some((x, x + 1))).take(3).toList)
  }

  test("ones via unfold") {
    import fpinscala.laziness.Stream._
    assertResult(List(1, 1, 1), "3 ones")(onesViaUnfold.take(3).toList)
  }

  test("constant infinite stream - via unfold") {
    import fpinscala.laziness.Stream._
    assertResult(List("a", "a", "a", "a", "a"), "constant a")(constantViaUnfold("a").take(5).toList)
  }

  test("from stream - via unfold") {
    import fpinscala.laziness.Stream._
    assertResult(List(5, 6, 7), "from 5")(fromViaUnfold(5).take(3).toList)
  }

  test("fibs - via unfold") {
    import fpinscala.laziness.Stream._
    assertResult(List(0, 1, 1, 2, 3, 5, 8), "fibs 7")(fibsViaUnfold.take(7).toList)
  }

  test("map - via unfold") {
    import fpinscala.laziness._

    assertResult(List(2, 4, 6), "base")(Stream(1, 2, 3).mapViaUnfold(_ * 2).toList)
    assertResult(List(), "empty")(Stream[Int]().mapViaUnfold(_ * 2).toList)
  }

  test("take - via unfold") {
    import fpinscala.laziness._

    assertResult(List(1, 2), "base")(Stream(1, 2, 3).takeViaUnfold(2).toList)
    assertResult(List(), "empty")(Stream().takeViaUnfold(0).toList)
  }

  test("takeWhile - via unfold") {
    import fpinscala.laziness._

    assertResult(List(1, 2), "base")(Stream(1, 2, 3).takeWhileViaUnfold(_ < 3).toList)
    assertResult(List(1, 2, 3), "base 2")(Stream(1, 2, 3).takeWhileViaUnfold(_ < 5).toList)
    assertResult(List(), "empty")(Stream[Int]().takeWhileViaUnfold(_ < 3).toList)
  }

  test("zipWith") {
    import fpinscala.laziness.Stream._
    import fpinscala.laziness._
    assertResult(List(1, 2, 2, 3, 4), "ones zipWith fibs")(ones.zipWith(fibs)(_ + _).take(5).toList)
    assertResult(List(4, 5, 6), "ones zipWith finite stream")(ones.zipWith(Stream(3, 4, 5))(_ + _).take(5).toList)
    assertResult(List(4, 5, 6), "finite stream zipWith ones")(Stream(3, 4, 5).zipWith(ones)(_ + _).take(5).toList)
    assertResult(List(), "empty zipWith ones")(empty[Int].zipWith(ones)(_ + _).take(5).toList)
  }

  test("zipAll") {
    import fpinscala.laziness.Stream._
    import fpinscala.laziness._

    assertResult(List((Some(1), Some(0)), (Some(1), Some(1))), "both infinite")(ones.zipAll(fibs).take(2).toList)
    assertResult(List((Some(1), Some(3)), (Some(1), None)), "infinite, finite")(ones.zipAll(Stream(3)).take(2).toList)
    assertResult(List((Some(3), Some(0)), (None, Some(1))), "finite, infinite")(Stream(3).zipAll(fibs).take(2).toList)
    assertResult(List((None, Some(0)), (None, Some(1))), "empty, infinite")(empty.zipAll(fibs).take(2).toList)
    assertResult(List((Some(1), None), (Some(1), None)), "infinite, empty")(ones.zipAll(empty).take(2).toList)
  }

  test("startsWith") {
    import fpinscala.laziness.Stream._
    import fpinscala.laziness._

    assert(Stream(1, 2, 3).startsWith(Stream(1, 2)), "base")
    assert(!Stream(1, 2).startsWith(Stream(1, 2, 3)), "shorter than argument")
    assert(!Stream(1, 2, 3).startsWith(Stream(2, 1)), "base false")
    assert(!empty[Int].startsWith(Stream(1, 2)), "empty")
  }

  test("tails") {
    import fpinscala.laziness.Stream._
    import fpinscala.laziness._
    assertResult(List(List(1, 2, 3), List(2, 3), List(3), List()), "base")(Stream(1, 2, 3).tails.toList.map(_.toList))
    assertResult(List(List()), "empty")(empty[Int].tails.toList.map(_.toList))
  }

  test("hasSubSequence") {
    import fpinscala.laziness.Stream._
    import fpinscala.laziness._
    assertResult(true, "1,2")(Stream(1, 2, 3, 4).hasSubSequence(Stream(1, 2)))
    assertResult(true, "2,3")(Stream(1, 2, 3, 4).hasSubSequence(Stream(2, 3)))
    assertResult(true, "4")(Stream(1, 2, 3, 4).hasSubSequence(Stream(4)))
    assertResult(false, "1,3")(Stream(1, 2, 3, 4).hasSubSequence(Stream(1, 3)))
    assertResult(false, "nil sup")(empty[Int].hasSubSequence(Stream(1, 3)))
    assertResult(true, "nil sub")(Stream(1, 2, 3, 4).hasSubSequence(empty[Int]))
    assertResult(true, "1 1 2 3 4, 1 2 3")(Stream(1, 2, 3, 4).hasSubSequence(Stream(1, 2, 3)))
  }

}
