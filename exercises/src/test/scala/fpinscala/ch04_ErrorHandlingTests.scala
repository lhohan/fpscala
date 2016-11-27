package fpinscala

import org.scalatest.FunSuite

/**
  * Created by hans on 25/10/14.
  */
class ch04_ErrorHandlingTests extends FunSuite {

  test("map") {
    import fpinscala.errorhandling._
    def f(x: Int) = x + 1

    assertResult(None, "none")(None.map(f))
    assertResult(Some(6), "some")(Some(5).map(f))
  }

  test("getOrElse") {
    import fpinscala.errorhandling._

    assertResult("x", "none")(None.getOrElse("x"))
    assertResult(5, "some")(Some(5).getOrElse(42))
  }

  test("flatMap") {
    import fpinscala.errorhandling._
    def f(x: Int) = Some(x + 1)

    assertResult(None, "none")(None.flatMap(f))
    assertResult(Some(6), "some")(Some(5).flatMap(f))
  }

  test("orElse") {
    import fpinscala.errorhandling._

    assertResult(Some("default option value"), "none")(None.orElse(Some("default option value")))
    assertResult(Some(5), "some")(
      Some(5).orElse(throw new IllegalArgumentException("I should not be evaluated")))
  }

  test("filter") {
    import fpinscala.errorhandling._

    assertResult(None, "none filter false")(None.filter(_ => false))
    assertResult(None, "none filter true")(None.filter(_ => true))
    assertResult(None, "some filter false")(Some(5).filter(_   % 2 == 0))
    assertResult(Some(6), "some filter true")(Some(6).filter(_ % 2 == 0))
  }

  test("variance") {
    import fpinscala.errorhandling.Option._
    import fpinscala.errorhandling._

    assertResult(Some(2.0), "1,2,3,4,5")(variance(Seq(1.0, 2, 3, 4, 5)))
    assertResult(None, "empty list") {
      variance(Seq())
    }
  }

  test("map2") {
    import fpinscala.errorhandling.Option._
    import fpinscala.errorhandling._

    assertResult(Some(3), "1 + 2")(map2(Some(1), Some(2))((a: Int, b: Int) => a + b))
    assertResult(None, "1 + none")(map2(Some(1), None)((a: Int, b: Int) => a + b))
    assertResult(None, "none + 1")(map2(None, Some(1))((a: Int, b: Int) => a + b))
  }

  test("sequence") {
    import fpinscala.errorhandling.Option._
    import fpinscala.errorhandling._

    assertResult(Some(List(1, 2, 4)), "some: 1,2,4")(sequence(List(Some(1), Some(2), Some(4))))
    assertResult(None, "none: first")(sequence(List(None, Some(2), Some(4))))
    assertResult(None, "none: last")(sequence(List(Some(1), Some(2), None)))
    assertResult(Some(List()), "sequence of empty list")(sequence(List()))
  }

  test("sequence_2") {
    import fpinscala.errorhandling.Option._
    import fpinscala.errorhandling._

    assertResult(Some(List(1, 2, 4)), "some: 1,2,4")(sequence_2(List(Some(1), Some(2), Some(4))))
    assertResult(None, "none: first")(sequence_2(List(None, Some(2), Some(4))))
    assertResult(None, "none: last")(sequence_2(List(Some(1), Some(2), None)))
    assertResult(Some(List()), "sequence of empty list")(sequence_2(List()))
  }

  test("sequence_3") {
    import fpinscala.errorhandling.Option._
    import fpinscala.errorhandling._

    assertResult(Some(List(1, 2, 4)), "some: 1,2,4")(sequence_3(List(Some(1), Some(2), Some(4))))
    assertResult(None, "none: first")(sequence_3(List(None, Some(2), Some(4))))
    assertResult(None, "none: last")(sequence_3(List(Some(1), Some(2), None)))
    assertResult(Some(List()), "sequence of empty list")(sequence_3(List()))
  }

  test("traverse") {
    import fpinscala.errorhandling.Option._
    import fpinscala.errorhandling._

    assertResult(Some(List(1, 2, 4)), "1,2,4")(traverse(List("1", "2", "4"))(s => Try(s.toInt)))
    assertResult(None, "1,two,4")(traverse(List("1", "two", "4"))(s => Try(s.toInt)))
  }

  test("traverse_2") {
    import fpinscala.errorhandling.Option._
    import fpinscala.errorhandling._

    assertResult(Some(List(1, 2, 4)), "1,2,4")(traverse_2(List("1", "2", "4"))(s => Try(s.toInt)))
    assertResult(None, "1,two,4")(traverse_2(List("1", "two", "4"))(s => Try(s.toInt)))
  }

  test("sequenceViaTraverse") {
    import fpinscala.errorhandling.Option._
    import fpinscala.errorhandling._

    assertResult(Some(List(1, 2, 4)), "some: 1,2,4")(
      sequenceViaTraverse(List(Some(1), Some(2), Some(4))))
    assertResult(None, "none: first")(sequenceViaTraverse(List(None, Some(2), Some(4))))
    assertResult(None, "none: last")(sequenceViaTraverse(List(Some(1), Some(2), None)))
    assertResult(Some(List()), "sequence of empty list")(sequenceViaTraverse(List()))
  }

  test("either - map") {
    import fpinscala.errorhandling._

    def f(x: Int) = x + 1

    assertResult(Right(5), "right")(Right(4).map(f))
    assertResult(Left("error"), "left")(Left("error").map(f))
  }

  test("either - flatMap") {
    import fpinscala.errorhandling._

    def f(x: Int) = Right(x + 1)

    assertResult(Right(5), "right")(Right(4).flatMap(f))
    assertResult(Left("error"), "left")(Left("error").flatMap(x => Left("error")))
  }

  test("either - orElse") {
    import fpinscala.errorhandling._

    def f(x: Int) = Right(x + 1)

    assertResult(Right(4), "right")(Right(4).orElse(f(4)))
    assertResult(Right(5), "right 2")(Left("error").orElse(f(4)))
    assertResult(Left("unexpected error"), "left")(Left("error").orElse(Left("unexpected error")))
  }

  test("either - map2") {
    import fpinscala.errorhandling._

    assertResult(Right(7), "right")(Right(4).map2(Right(3))(_ + _))
    assertResult(Left("error"), "left 1")(
      (Left("error"): Either[String, Int]).map2[String, Int, Int](Right(3))(_ + _))
    assertResult(Left("error2"), "left 2")(Right(1).map2[String, Int, Int](Left("error2"))(_ + _))
  }

  test("either - sequence") {
    import fpinscala.errorhandling.Either._
    import fpinscala.errorhandling._

    assertResult(Right(List(4, 2, 1)), "right")(sequence(List(Right(4), Right(2), Right(1))))
    assertResult(Left("error"), "error")(sequence(List(Left("error"), Right(2), Right(1))))
    assertResult(Left("error"), "error 2")(sequence(List(Right(1), Right(2), Left("error"))))
    assertResult(Right(List()), "empty")(sequence(List()))
  }

  test("either - traverse") {
    import fpinscala.errorhandling.Either._
    import fpinscala.errorhandling._

    assertResult(Right(List(4, 2, 1)), "right")(traverse(List("4", "2", "1"))((s: String) =>
      Try(s.toInt)))

    val result = traverse(List("4", "two", "1"))((s: String) => Try(s.toInt))
    assert(result.isInstanceOf[Left[Exception]], "error")

    assertResult(Right(List()), "empty")(traverse(List())((s: String) => Try(s.toInt)))
  }

  test("either - sequence via traverse") {
    import fpinscala.errorhandling.Either._
    import fpinscala.errorhandling._

    assertResult(Right(List(4, 2, 1)), "right")(
      sequenceViaTraverse(List(Right(4), Right(2), Right(1))))
    assertResult(Left("error"), "error")(
      sequenceViaTraverse(List(Left("error"), Right(2), Right(1))))
    assertResult(Left("error"), "error 2")(
      sequenceViaTraverse(List(Right(1), Right(2), Left("error"))))
    assertResult(Right(List()), "empty")(sequenceViaTraverse(List()))
  }

}
