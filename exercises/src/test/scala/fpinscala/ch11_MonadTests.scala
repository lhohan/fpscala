package fpinscala

import java.util.concurrent.Executors

import fpinscala.monads.Monad._
import fpinscala.parallelism.Nonblocking.Par
import org.scalatest.FunSuite

import scala.util.Try

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

  // more info http://typelevel.org/cats/tut/traverse.html
  test("11.3 - sequence") {
    assert(Some(List(1, 2, 3)) == optionMonad.sequence(List(Option(1), Option(2), Option(3))))
    assert(None == optionMonad.sequence(List(Option(1), None, Option(3))))

    assert(List(List(1, 2, 3)) == listMonad.sequence(List(List(1), List(2), List(3))))
    assert(List() == listMonad.sequence(List(List(1, 2, 3), List(), List(4, 5), List(6, 7, 8))))
    assert(List(
      List(1, 4, 6),
      List(1, 4, 7),
      List(1, 4, 8),
      List(1, 5, 6),
      List(1, 5, 7),
      List(1, 5, 8),
      List(2, 4, 6),
      List(2, 4, 7),
      List(2, 4, 8),
      List(2, 5, 6),
      List(2, 5, 7),
      List(2, 5, 8),
      List(3, 4, 6),
      List(3, 4, 7),
      List(3, 4, 8),
      List(3, 5, 6),
      List(3, 5, 7),
      List(3, 5, 8)
    ) == listMonad.sequence(List(List(1, 2, 3), List(4, 5), List(6, 7, 8))))

  }

  test("11.3 - traverse") {
    def parseInt(s: String) = Try(s.toInt).toOption

    assert(Some(List(1, 2, 3)) == optionMonad.traverse(List("1", "2", "3"))(parseInt))
    assert(None == optionMonad.traverse(List("1", "abc", "3"))(parseInt))
  }

  test("11.4 - replicateM") {

    assert(Some(List(1, 1, 1)) == optionMonad.replicateM(3, Some(1)))
    assert(List(
      List(1, 1),
      List(1, 2),
      List(1, 3),
      List(2, 1),
      List(2, 2),
      List(2, 3),
      List(3, 1),
      List(3, 2),
      List(3, 3)
    ) == listMonad.replicateM(2, List(1, 2, 3)))
  }

  test("11.6 - filterM") {

    def evenOption(i: Int) = Some(i % 2 == 0)
    assert(Some(List(2, 4)) == optionMonad.filterM(List(1, 2, 3, 4, 5))(evenOption))
    assert(None == optionMonad.filterM(List(1, 2, 3, 4, 5)) { (i: Int) => if (i == 4) None else Some(true) })

    def evenList(i: Int) = List(i % 2 == 0)
    assert(List(
      List(2, 4)
    ) == listMonad.filterM(List(1, 2, 3, 4, 5))(evenList))
  }

  test("11.7 - compose") {

    def f: String => List[Int] = s => Try(s.toInt).toOption.toList
    def g: Int => List[Int] = i => List.fill(3)(i)
    def h = listMonad.compose(f, g)

    assert(List(1, 1, 1) == h("1"))
    assert(List() == h("abc"))
  }
  test("11.8 - flatMap in terms of compose") {
    assert(Some(6) == optionMonad._flatMap(Some(5))(i => Some(i + 1)))
    assert(None == optionMonad._flatMap(Some(5))(i => None))
    assert(None == optionMonad._flatMap(None.asInstanceOf[Option[Int]])(i => Some(i + 1)))
  }

}
