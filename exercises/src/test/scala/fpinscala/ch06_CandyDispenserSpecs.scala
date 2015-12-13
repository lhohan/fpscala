package fpinscala

import fpinscala.state.State._
import fpinscala.state.{ Coin, Input, Machine, Turn }
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.{ BooleanOperators, forAll }
import org.scalacheck.{ Arbitrary, Gen, Properties }

class ch06_CandyDispenserSpecs extends Properties("Candy dispenser machine") {
  property("Inserting a coin in locked machine if candy left should unlock") = forAll { (machine: Machine) =>
    (machine.locked && machine.candies > 0) ==> {
      val (_, resultMachine) = simulateMachine(List(Coin)).run(machine)
      !resultMachine.locked
    }
  }
  property("Turning the knob on an unlocked machine should dispense a candy and become locked") = forAll { (machine: Machine) =>
    (!machine.locked && machine.candies > 0) ==> {

      val ((_, resultCandies), resultMachine) = simulateMachine(List(Turn)).run(machine)
      val startCandies = machine.candies
      (resultCandies == startCandies - 1) :| "result # candies not one less than start candies" &&
        resultMachine.locked :| "machine not locked"
    }
  }

  property("Turning the knob on a locked machine should do nothing") = forAll { (machine: Machine) =>
    machine.locked ==> {

      val ((coins, candies), resultMachine) = simulateMachine(List(Turn)).run(machine)
      (machine.candies == candies) :| "coins not equal" &&
        (machine.coins == coins) :| "candies not equal" &&
        (machine == resultMachine) :| "machines not equal"
    }
  }

  property("Inserting a coin into an unlocked machine should do nothing") = forAll { (machine: Machine) =>
    (!machine.locked) ==> {

      val ((coins, candies), resultMachine) = simulateMachine(List(Coin)).run(machine)

      (machine.candies == candies) :| "coins not equal" &&
        (machine.coins == coins) :| "candies not equal" &&
        (machine == resultMachine) :| "machines not equal"
    }
  }

  property("Inserting a machine that's out of candy should ignore all inputs") = forAll {
    (machine: Machine, input: Input) =>
      (machine.candies <= 0) ==> {

        val ((coins, candies), resultMachine) = simulateMachine(List(input)).run(machine)
        (machine.candies == candies) :| "coins not equal" &&
          (machine.coins == coins) :| "candies not equal" &&
          (machine == resultMachine) :| "machines not equal"
      }
  }

  // If we want 0 candies for example, just putting a condition on generated machines will require too many generations
  // to actually satisfy the condition. So here we create a generator the is biased to zero.
  lazy val moreFalsesThanTrues: Gen[Boolean] = Gen.frequency((4, false), (1, true))
  lazy val zeroBiased: Gen[Int] = moreFalsesThanTrues.flatMap { b =>
    if (b) {
      Gen.const(0)
    } else {
      Gen.chooseNum(1, 100)
    }
  }

  // A generator of Machines.
  implicit lazy val arbMachine: Arbitrary[Machine] =
    Arbitrary(
      for {
        coins <- zeroBiased
        candies <- zeroBiased
        locked <- arbitrary[Boolean]
      } yield Machine(locked = locked, candies = candies, coins = coins)
    ) // A generator of Machines.

  implicit lazy val arbInput: Arbitrary[Input] =
    Arbitrary(
      for {
        input <- Gen.oneOf[Input](Turn, Coin)
      } yield input
    )
}
