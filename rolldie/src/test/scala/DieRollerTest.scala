import org.scalatest.FunSuite

/**
 * Created by hans on 16/11/14.
 */
class DieRollerTest extends FunSuite {

  import DieRoller._

  test("roll should be between in range 1 to 6") {
    val _3Rolls = Stream.iterate(rollDie(Simple(5)), 3) {
      x =>
        val (i, r) = x
        val (i2, r2) = rollDie(r)
        (i, r2)
    }.map(_._1).toList

    _3Rolls.foreach { roll =>
      assert(roll > 0 && roll <= 6, s"error: $roll")
    }
  }
}
