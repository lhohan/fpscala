package fpinscala

import java.util.concurrent.Executors

import fpinscala.monads.{Id, Reader}
import fpinscala.monads.Monad._
import fpinscala.parallelism.Nonblocking.Par
import fpinscala.state.State
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

  test("11.12 - join") {
    assert(Some(5) == optionMonad.join(Some(Some(5))))
    assert(None == optionMonad.join(None))

    assert(List(1, 2, 3, 4, 5) == listMonad.join(List(List(1, 2, 3), List(4, 5))))
    assert(List(1, 2, 3, 4, 5) == listMonad.join(List(List(), List(1, 2, 3, 4, 5))))
    assert(List() == listMonad.join(List(List())))
    assert(List() == listMonad.join(List()))
  }

  test("11.13 - flatMap in terms of join") {
    assert(Some(6) == optionMonad.__flatMap(Some(5))(i => Some(i + 1)))
    assert(None == optionMonad.__flatMap(Some(5))(i => None))
    assert(None == optionMonad.__flatMap(None.asInstanceOf[Option[Int]])(i => Some(i + 1)))
  }

  test("11.17 - id monad") {
    val i6: Id[Int] = idMonad.unit(6)
    val ii6 = idMonad.unit(i6)
    assert(Id(6) == idMonad.join(ii6))
    import scala.language.implicitConversions
    implicit def toId[A](a: A): Id[A] = Id(a)
    val x = for {
      a <- 6
      b <- 7
    } yield a + b
    assert(13 == x.value)
  }

  test("11.18 - state monad: sequence") {
    // sequence applies all single state transition into a cumulative state transition.
    type myState[A] = State[Int, A]
    def doCount(s: String): myState[String] = State {
      counter => (s + " executed!", counter + 1)
    }

    val cmds = List("cd", "ls", "pwd")
    val states: List[myState[String]] = cmds.map { cmd => doCount(cmd) }
    val sequence: State[Int, List[String]] = stateMonad.sequence(states)
    val counter = sequence

    val result: (List[String], Int) = counter.run(0)
    val strings = result._1
    assert(List("cd executed!", "ls executed!", "pwd executed!") == strings)
    val totalCounted_state = result._2
    assert(3 == totalCounted_state, "sequence of accumulated single incremental counts should result in the total count")
  }

  test("11.19 - state monad: replicateM") {
    type myState[A] = State[Int, A]
    def doCount(s: String): myState[String] = State {
      counter => (s.reverse, counter + 1)
    }

    val cmds = List("cd", "ls", "open chrome")
    val counter = stateMonad.replicateM(5, doCount("abc"))
    val result: (List[String], Int) = counter.run(0)
    assert(List("cba", "cba", "cba", "cba", "cba") == result._1)
    val totalCounted_state = result._2
    assert(5 == totalCounted_state, "replicate M applies the same state transition n times")
  }

  test("11.20 - state laws") {
    val F = stateMonad[Int]

    // law: running the state monad with initial state, no other transitions, returns the initial state
    val r1 = for {
      s <- getState[Int]
    } yield s
    assert(5 == r1.run(5)._2, "law: running the state monad with initial state, no other transitions, returns the initial state")

    val r2 = for {
      _ <- setState[Int](3)
      _ <- setState[Int](2)
      _ <- setState[Int](1)
      s <- getState
    } yield s
    assert(1 == r2.run(0)._2, "law: setting the state, should return the last set state when getting the state")

    val r3 = for {
      x <- F.unit("abc")
    } yield x
    assert("abc" == r3.run(2)._1, "law: unit value should be returned when 'directly' running the state monad.")

  }

  test("11.21 - Reader monad") {
    val replicated = readerMonad.replicateM(3, Reader[Int, Int](x => x + 10))
    assert(List(12, 12, 12) == replicated.run(2), "replicating of the reader monad: apply function n times with same arg and return the result")

    val readers: List[Reader[Int, Int]] = List(Reader(x => (x + 1) * 2), Reader(x => x / 2))
    val sequenced: Reader[Int, List[Int]] = readerMonad.sequence(readers)
    assert(List(10, 2) == sequenced.run(4), "sequence turn a list of functions in to a function that takes one argument and returns the results in a list")

  }

}
