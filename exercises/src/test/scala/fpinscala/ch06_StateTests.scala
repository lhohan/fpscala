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

}