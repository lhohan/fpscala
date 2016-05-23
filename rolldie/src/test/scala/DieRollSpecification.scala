
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.forAll
import org.scalacheck.{ Arbitrary, Properties }

object DieRollSpecification extends Properties("Die Roll") {

  import DieRoller._

  property("Range of die roll should be in range 1 to 6") = forAll { (rng: RNG) =>
    val (dieRoll, _) = rollDie(rng)
    dieRoll <= 6 && dieRoll >= 1
  }

  // A generator of RNG's.
  implicit lazy val arbRNG: Arbitrary[RNG] =
    Arbitrary(
      for {
        seed <- arbitrary[Int]
      } yield Simple(seed)
    )

}
