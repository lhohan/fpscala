

/**
 * Created by hans on 16/11/14.
 */
object DieRoller {

  // The key to recovering referential transparency: make state updates explicit.
  // Return the new state along with the value that we are generating
  trait RNG {
    def nextInt: (Int, RNG)
  }

  // Implementation of RNG that will do the trick. That's all we need for now. Detail in the book.
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  // keeping it simple, book's impl. is different but more complex
  def nonNegativeIntLessThan(n: Int)(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    ((if (i < 0) -i else i) % n, r)
  }

  def rollDie: RNG => (Int, RNG) = nonNegativeIntLessThan(6)

}
