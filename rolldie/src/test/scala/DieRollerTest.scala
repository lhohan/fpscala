import org.scalatest.FunSuite

/**
 * Created by hans on 16/11/14.
 */
class DieRollerTest extends FunSuite {

  import DieRoller._

  test("roll should be in range 1 to 6") {

    // Check reproducible result reported by the property-based test.
    // The seed may be different.
    val dieRollResults = rollDie(Simple(-2147483648))
    val result = dieRollResults._1

    assert(result >= 1 && result <= 6, s"error: $result")
  }
}
