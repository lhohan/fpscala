package fpinscala

import org.scalatest.FunSuite

import fpinscala.streamingio.SimpleStreamTransducers._

class ch15_StreamingIOTests extends FunSuite {
  test("15.1: take") {
    val p: Process[Int, Int] = Process.take(3)
    assert(List(1, 2, 3) == p(Stream(1, 2, 3, 4, 5)).toList)
    assert(List(1) == p(Stream(1)).toList)
    assert(List() == p(Stream()).toList)
  }

  test("15.1: drop") {
    val p: Process[Int, Int] = Process.drop(3)
    assert(List(4, 5) == p(Stream(1, 2, 3, 4, 5)).toList)
    assert(List() == p(Stream(1)).toList)
    assert(List() == p(Stream()).toList)
  }

  test("15.1: takeWhile") {
    val p: Process[Int, Int] = Process.takeWhile(_ < 3)
    assert(List(1, 2) == p(Stream(1, 2, 3, 4, 5)).toList)
    assert(List(1, 1, 0) == p(Stream(1, 1, 0)).toList)
    assert(List() == p(Stream()).toList)
    assert(List() == p(Stream(5, 1, 2, 3)).toList)
  }

  test("15.1: dropWhile") {
    val p: Process[Int, Int] = Process.dropWhile(_ < 3)
    assert(List(3, 4, 5) == p(Stream(1, 2, 3, 4, 5)).toList)
    assert(List() == p(Stream(1, 1, 0)).toList)
    assert(List() == p(Stream()).toList)
    assert(List(5, 1, 2, 3) == p(Stream(5, 1, 2, 3)).toList)
  }

  test("15.2: count") {
    val p: Process[String, Int] = Process.count
    assert(List(0, 1, 2, 3) == p(Stream("a", "b", "c")).toList)
    assert(List(0, 1) == p(Stream("a")).toList)
    assert(List(0) == p(Stream.empty[String]).toList)
  }

  test("15.3: mean") {
    val p: Process[Double, Double] = Process.mean
    assert(List(1.0, 1.0, 1.0) == p(Stream(1.0, 1.0, 1.0)).toList)
    assert(List(1.0, 2.0, 3.0) == p(Stream(1.0, 3.0, 5.0)).toList)
    assert(List(5.0) == p(Stream(5.0)).toList)
    assert(List() == p(Stream()).toList)
  }

  test("15.4: count3") {
    val p: Process[String, Int] = Process.count3
    assert(List(1, 2, 3) == p(Stream("a", "b", "c")).toList)
    assert(List(1) == p(Stream("a")).toList)
    assert(List() == p(Stream.empty[String]).toList)
  }

  test("15.4: sum2") {
    val p: Process[Double, Double] = Process.sum2
    assert(List(1.0, 2.0, 3.0) == p(Stream(1.0, 1.0, 1.0)).toList)
    assert(List(1.0, 4.0, 9.0) == p(Stream(1.0, 3.0, 5.0)).toList)
    assert(List(5.0) == p(Stream(5.0)).toList)
    assert(List() == p(Stream()).toList)
  }

  test("15.5: compose") {
    val p1: Process[Double, Double] = Process.filter(_ > 5)
    val p2: Process[Double, Double] = Process.sum
    val p: Process[Double, Double]  = p1 |> p2
    val q: Process[Double, Double]  = p2 |> p1
    assert(List(6.0, 16.0) == p(Stream(1.0, 6.0, 10.0, 1.0, 1.0, 4.0)).toList)
    // note: first element '1.0' is filtered
    assert(List(7.0, 17.0, 18.0, 19.0, 23.0) == q(Stream(1.0, 6.0, 10.0, 1.0, 1.0, 4.0)).toList)
  }

  test("15.5: compose - finite p1 , infinite p2") {
    import Process._
    val infiniteP: Process[Int, Int] = Process.count
    val finiteP: Process[Int, Int]   = Emit(25, Halt())
    val p: Process[Int, Int]         = infiniteP |> finiteP
    val q: Process[Int, Int]         = finiteP |> infiniteP
    assert(List(25) == p(Stream.from(500)).toList)
    assert(List(0, 1) == q(Stream.from(500)).toList)
  }

  test("15.5: compose alternate") {
    val p1: Process[Double, Double] = Process.filter(_ > 5)
    val p2: Process[Double, Double] = Process.sum
    val p: Process[Double, Double]  = p1 pipe2 p2
    val q: Process[Double, Double]  = p2 pipe2 p1
    assert(List(6.0, 16.0) == p(Stream(1.0, 6.0, 10.0, 1.0, 1.0, 4.0)).toList)
    // note: first element '1.0' is filtered
    assert(List(7.0, 17.0, 18.0, 19.0, 23.0) == q(Stream(1.0, 6.0, 10.0, 1.0, 1.0, 4.0)).toList)
  }

  test("15.5: compose alternate - finite p1 , infinite p2") {
    import Process._
    val infiniteP: Process[Int, Int] = Process.count
    val finiteP: Process[Int, Int]   = Emit(25, Halt())
    val p: Process[Int, Int]         = infiniteP pipe2 finiteP
    val q: Process[Int, Int]         = finiteP pipe2 infiniteP
    assert(List(25) == p(Stream.from(500)).toList)
    assert(List(0, 1) == q(Stream.from(500)).toList)
  }

  test("15.6: zipWithIndex") {
    val p: Process[Double, (Double, Int)] = Process.sum.zipWithIndex
    assert(
      List((1.0, 0), (7.0, 1), (17.0, 2), (18.0, 3), (19.0, 4), (23.0, 5))
        ==
          p(Stream(1.0, 6.0, 10.0, 1.0, 1.0, 4.0)).toList
    )
    assert(List.empty[(Double, Int)] == p(Stream.empty[Double]).toList)
  }

  test("15.6: zipWithIndex - one Emit") {
    import Process._
    val p: Process[Int, (Int, Int)] = Emit[Int, Int](25, Halt()).zipWithIndex
    assert(
      List((25, 0))
        ==
          p(Stream.from(5)).toList
    )
  }

  test("15.6: zipWithIndex - one Await sending Emit") {
    import Process._
    val p: Process[Int, (Int, Int)] = Await[Int, Int] { _ =>
      Emit(25, Halt())
    }.zipWithIndex
    assert(
      List((25, 0))
        ==
          p(Stream.from(5)).toList
    )
  }

  test("15.6: zipWithIndex - one Await sending Halt") {
    import Process._
    val p: Process[Int, (Int, Int)] = Await[Int, Int] { _ =>
      Halt()
    }.zipWithIndex
    assert(
      List()
        ==
          p(Stream.from(5)).toList
    )
  }

  test("15.7: zip") {
    import Process._
    val p: Process[Double, (Double, Int)] = zip(sum, count)
    assert(
      List((1.0, 0), (7.0, 1), (17.0, 2), (18.0, 3), (19.0, 4), (23.0, 5))
        ==
          p(Stream(1.0, 6.0, 10.0, 1.0, 1.0, 4.0)).toList
    )
    assert(List.empty[(Double, Int)] == p(Stream.empty[Double]).toList)
  }

  test("15.7: zip - one Emit") {
    import Process._
    val p: Process[Int, (Int, Int)] = zip(Emit[Int, Int](25, Halt()), count)
    assert(
      List((25, 0))
        ==
          p(Stream.from(5)).toList
    )
  }

  test("15.7: meanWithZip") {
    import Process._
    val p = meanWithZip
    assert(
      List(1.0, 1.5, 2.0)
        ==
          p(Stream(1.0, 2.0, 3.0)).toList
    )
  }

  test("15.7: meanWithZip - empty") {
    import Process._
    val p = meanWithZip
    assert(
      List()
        ==
          p(Stream()).toList
    )
  }

  test("15.8: exists - true") {
    import Process._
    val p = exists { s: String =>
      s.startsWith("a")
    }
    assert(
      List(false, true, true)
        ==
          p(Stream("bbbb", "aaaa", "bbbb")).toList
    )
  }

  test("15.8: exists - false") {
    import Process._
    val p = exists { s: String =>
      s.startsWith("a")
    }
    assert(
      List(false, false, false)
        ==
          p(Stream("bbbb", "bbb", "bbbb")).toList
    )
  }

  test("15.8: exists - empty") {
    import Process._
    val p = exists { s: String =>
      s.startsWith("a")
    }
    assert(
      List()
        ==
          p(Stream()).toList
    )
  }

  test("15.8: existsResult - true") {
    import Process._
    val p = existsResult { s: String =>
      s.startsWith("a")
    }
    assert(
      List(true)
        ==
          p(Stream("bbbb", "aaaa", "bbbb")).toList
    )
  }

  test("15.8: existsResult - false") {
    import Process._
    val p = existsResult { s: String =>
      s.startsWith("a")
    }
    assert(
      List(false)
        ==
          p(Stream("bbbb", "", "bbbb")).toList
    )
  }

  test("15.8: existsResult - empty") {
    import Process._
    val p = existsResult { s: String =>
      s.startsWith("a")
    }
    assert(
      List(false)
        ==
          p(Stream()).toList
    )
  }

}
