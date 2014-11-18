import org.scalatest.FunSuite

/**
 * Created by hans on 16/11/14.
 */
class DieRollerTest extends FunSuite {

  import DieRoller._

  test("roll should be in range 1 to 6") {

    def rolls(n: Int)(rng: RNG): (List[Int], RNG) = {
      def loop(i: Int, r: RNG, acc: List[Int]): (List[Int], RNG) = i match {
        case 0 => (acc, r)
        case _ =>
          val (next, nextRng) = rollDie(rng)
          loop(i - 1, nextRng, next :: acc)
      }
      loop(n, rng, List())
    }

    val _3Rolls = rolls(3)(Simple(5))._1

    _3Rolls.foreach { roll =>
      assert(roll > 0 && roll <= 6, s"error: $roll")
    }
  }
}
