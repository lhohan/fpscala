import fpinscala.state.RNG
import org.scalatest.FunSuite

/**
 * Created by hans on 29/10/14.
 */
class ch06_StateTests extends FunSuite {

  test("nonNegativeInt") {
    import fpinscala.state.RNG._

    def seed = Simple(37)
    val rnds = Stream.iterate(nonNegativeInt(seed))(r => nonNegativeInt(r._2))

    rnds.take(100).toList.foreach { x =>
      assert(x._1 >= 0, s"should be positive ${x._1}")
    }
  }

  test("double") {
    import fpinscala.state.RNG._

    def seed = Simple(37)
    val rnds = Stream.iterate(double(seed))(r => double(r._2))

    rnds.take(100).toList.foreach { x =>
      assert(x._1 >= 0 && x._1 < 1, s"should be between 0 and 1 (strictly smaller) ${x._1}")
    }
  }

  // not sure about following tests only check on type?
  test("intDouble") {
    import fpinscala.state.RNG._

    def seed = Simple(37)
    val rnds = Stream.iterate(intDouble(seed))(r => intDouble(r._2))

    rnds.take(100).toList.foreach {
      case x: ((Int, Double), RNG) => // OK
      case x @ _ => fail(s"$x")
    }
  }

  test("doubleInt") {
    import fpinscala.state.RNG._

    def seed = Simple(37)
    val rnds = Stream.iterate(doubleInt(seed))(r => doubleInt(r._2))

    rnds.take(100).toList.foreach {
      case x: ((Double, Int), RNG) => // OK
      case x @ _ => fail(s"$x")
    }
  }

  test("double3") {
    // similar to above
  }

  test("ints") {
    import fpinscala.state.RNG._

    val (is, _) = ints(5)(Simple(37))
    //    is.foreach(i => println(i))
  }

  test("doubleElegant") {
    import fpinscala.state.RNG._

    def seed = Simple(37)
    val rnds = Stream.iterate(doubleElegant(seed))(r => doubleElegant(r._2))

    rnds.take(100).toList.foreach { x =>
      assert(x._1 >= 0 && x._1 < 1, s"should be between 0 and 1 (strictly smaller) ${x._1}")
    }
  }

  test("map2") {
    import fpinscala.state.RNG._

    val composedRnd = map2(nonNegativeInt, double)((i, d) => s"$i & $d")

    val rnds = Stream.iterate(composedRnd(Simple(37)))(r => composedRnd(r._2))

    //    rnds.take(5).foreach(x => println(x._1))
  }

  test("sequence") {
    import fpinscala.state.RNG._

    val composedRnd = sequence(List(nonNegativeInt _, double _))

    val rnds = Stream.iterate(composedRnd(Simple(37)))(r => composedRnd(r._2))

    //    rnds.take(5).foreach(x => println(x._1))
  }

  test("ints via seq") {
    import fpinscala.state.RNG._

    val (is, _) = intsViaSequence(5)(Simple(37))
    //    is.foreach(i => println(i))
  }

  test("non neg les than") {
    import fpinscala.state.RNG._

    val (is, _) = nonNegativeLessThan(5)(Simple(37))
    val rnds = Stream.iterate(nonNegativeLessThan(10)(Simple(37)))(r => nonNegativeLessThan(10)(r._2))

    rnds.take(10).foreach(x => println(x._1))
  }

}