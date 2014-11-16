import scala.util.Random

/**
 * Created by hans on 16/11/14.
 */
object DieRoller {
  def rollDie: Int = {
    val rng = Random
    rng.nextInt(6)
  }
}
