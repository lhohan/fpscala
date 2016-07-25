package fpinscala

import java.util.concurrent.Executors

import fpinscala.monoids._
import fpinscala.monoids.Monoid._
import fpinscala.parallelism.Nonblocking.Par
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

  test("ex 10.6 foldRight") {
    assert("12345" == foldRight(List(1, 2, 3, 4))("5")((a, b) => a + b))
    assert(5 == foldRight(List.empty[Int])(5)((a, b) => a + b))
  }

  test("ex 10.6 foldLeft") {
    assert("54321" == foldLeft(List(1, 2, 3, 4))("5")((a, b) => a + b))
  }

  test("ex 10.7 foldMapV") {
    assert(80 == foldMapV(IndexedSeq(1, 2, 3, 4, 6, 7, 8, 9), intAddition)(_ * 2))
    assert(0 == foldMapV(IndexedSeq(), intAddition)(identity[Int]))
    assert(10 == foldMapV(IndexedSeq(5), intAddition)(_ * 2))
  }

  test("ex 10.8 parFoldMap") {
    val es = Executors.newFixedThreadPool(4)
    assert(80 == Par.run(es)(parFoldMap(IndexedSeq(1, 2, 3, 4, 6, 7, 8, 9), intAddition)(_ * 2)))
    assert(0 == Par.run(es)(parFoldMap(IndexedSeq(), intAddition)(identity[Int])))
    assert(10 == Par.run(es)(parFoldMap(IndexedSeq(5), intAddition)(_ * 2)))
  }

  test("ex 10.9 ordered") {
    assert(ordered(IndexedSeq(1, 2, 3, 4, 6, 7, 8, 9)))
    assert(ordered(IndexedSeq()))
    assert(!ordered(IndexedSeq(1, 2, 3, 100, 6, 7, 8, 9)))
    assert(!ordered(IndexedSeq(100, 1, 2, 3, 6, 4, 7, 8, 9)))
  }

  test("ex 10.10, 10.11 word count") {
    assert(16 == count("ScalaTest provides a domain specific language (DSL) for expressing assertions in tests using the word should."))
    assert(1 == count("ScalaTest "))
    assert(1 == count(" ScalaTest "))
    assert(1 == count(" ScalaTest"))
    assert(0 == count("     "))
  }

  test("ex 10.12 Foldable[List], etc") {
    assert("12345" == ListFoldable.foldMap(List(1, 2, 3, 4, 5))(_.toString)(stringMonoid))
    assert("12345" == IndexedSeqFoldable.foldMap(IndexedSeq(1, 2, 3, 4, 5))(_.toString)(stringMonoid))
    assert("12345" == StreamFoldable.foldMap(Stream(1, 2, 3, 4, 5))(_.toString)(stringMonoid))

  }

  test("ex 10.13 TreeFoldable") {
    assert("1024" == TreeFoldable.foldLeft(Branch(Leaf(5), Branch(Leaf(1), Leaf(2))))("")((s, x) => s + x * 2))
    assert("10" == TreeFoldable.foldLeft(Leaf(5))("")((s, x) => s + x * 2))
    assert("1024" == TreeFoldable.foldRight(Branch(Leaf(5), Branch(Leaf(1), Leaf(2))))("")((x, s) => s + x * 2))

  }

  test("ex 10.14 OptionFoldable") {
    assert(15 == OptionFoldable.foldLeft(Some(5))(10) { (a, b) => a + b })
    assert(10 == OptionFoldable.foldLeft(None.asInstanceOf[Option[Int]])(10) { (a, b) => a + b })
    assert(15 == OptionFoldable.foldRight(Some(5))(10) { (a, b) => a + b })
    assert(10 == OptionFoldable.foldRight(None.asInstanceOf[Option[Int]])(10) { (a, b) => a + b })
    assert("5-" == OptionFoldable.foldMap(Some(5))(_ + "-")(stringMonoid))
    assert("" == OptionFoldable.foldMap(None.asInstanceOf[Option[Int]])(_ + "-")(stringMonoid))
  }

  test("ex 10.15 generic toList") {
    assert(List(5) == OptionFoldable.toList(Some(5)))
    assert(List.empty[Int] == OptionFoldable.toList(None.asInstanceOf[Option[Int]]))
    assert(List(5, 1, 2) == TreeFoldable.toList(Branch(Leaf(5), Branch(Leaf(1), Leaf(2)))))
    assert(List(5) == TreeFoldable.toList(Leaf(5)))
  }

  test("ex 10.16 product monoid") {
    def pairGen[A, B](ga: Gen[A], gb: Gen[B]): Gen[(A, B)] = ga.flatMap(a => gb.map(b => (a, b)))

    Prop.run(monoidLaws(productMonoid(intAddition, stringMonoid), pairGen(Gen.smallInt, Gen.stringN(10))))
  }
}
