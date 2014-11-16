import org.scalatest.FunSuite

/**
 * Created by hans on 16/11/14.
 */
class DieRollerTest extends FunSuite {
  import DieRoller._

  test("roll should be between in range 1 to 6") {
    val _3Rolls = (1 to 3).map(x => rollDie)
    _3Rolls.foreach { roll =>
      assert(roll > 0 && roll <= 6, s"error: $roll")
    }
  }
}
