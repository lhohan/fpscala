import org.scalatest._
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.Span

import scala.language.postfixOps

class Par2Tests extends FunSuite with Timeouts {
  import fpinscala.parallelism.Par2._
  import scala.concurrent.duration._
  import scala.concurrent._

  test("parMap") {
    implicit val ec = scala.concurrent.ExecutionContext.global
    val p = parMap(List(3, 2, 1)) { i =>
      Thread.sleep(i * 1000);
      //println(i);
    }

    // we wait less than 3 + 2 + 1 seconds to check if parMap indeed runs in parallel
    Await.result(p.run, 3.5 seconds)
  }

  test("parSum") {
    implicit val ec = scala.concurrent.ExecutionContext.global
    import fpinscala.parallelism.Examples2._

    val p = parSum(Vector(100, 200, 300))
    assert(600 === Await.result(p.run, 100 millis))

  }

  test("parSumExec") {
    implicit val ec = scala.concurrent.ExecutionContext.global
    import fpinscala.parallelism.Examples2._

    val p = parSumExec(Vector(100, 200, 300))
    assert(600 === Await.result(p.run, 100 millis))
  }

  test("parMaxExec") {
    implicit val ec = scala.concurrent.ExecutionContext.global
    import fpinscala.parallelism.Examples2._

    val p = parMaxExec(Vector(100, 200, 300))
    assert(Some(300) === Await.result(p.run, 100 millis))
    assert(None === Await.result(parMaxExec(Vector()).run, 100 millis))
  }

  test("wordCount") {
    implicit val ec = scala.concurrent.ExecutionContext.global
    import fpinscala.parallelism.Examples2._

    val p = wordCount(List("This is what you choose.", "You could be good today.", "Instead you choose tomorrow.", "Marcus Aurelius"))
    assert(Some(16) === Await.result(p.run, 100 millis))
    assert(None === Await.result(wordCount(Nil).run, 100 millis))
  }

}
