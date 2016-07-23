package fpinscala

import fpinscala.monoids.Monoid._
import fpinscala.testing._
import org.scalatest.FunSuite

class ch10_MonoidsTests extends FunSuite {

  test("ex 10.1") {
    assert(intAddition.op(1, 2) == 3)
  }

  test("test monoids using laws") {
    Prop.run(monoidLaws(intAddition, Gen.smallInt))
    Prop.run(monoidLaws(intMultiplication, Gen.smallInt))
    Prop.run(monoidLaws(booleanAnd, Gen.boolean))
    Prop.run(monoidLaws(booleanOr, Gen.boolean))

    def optionGen[A](g: Gen[A]): Gen[Option[A]] = g.flatMap(a => Gen.boolean.map(b => if (b) Some(a) else None))
    Prop.run(monoidLaws(optionMonoid[Int], optionGen(Gen.smallInt)))

    // TODO check below property: fails
    def endoGen(g: Gen[Int]): Gen[Int => Int] = g.map { a: Int => x: Int => a * x }
    //    def endoGen(g: Gen[Int]): Gen[Int => Int] = g.flatMap { a: Int => Gen.boolean.map(b => if (b) { x: Int => a * x } else { x: Int => a + x }) }
    Prop.run(monoidLaws(endoMonoid[Int], endoGen(Gen.smallInt)))
  }

  test("ex 10.5 foldMap") {
    assert(14 == foldMap(List(1, 2, 3, 4), intAddition)(_ + 1))
  }
}
