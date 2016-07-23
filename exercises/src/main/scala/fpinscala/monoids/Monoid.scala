package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._

import scala.language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1 compose a2

    override def zero: A => A = identity[A]
  }

  // TODOX: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  //  trait Prop {}

  // TODOX: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    def associativityLaw = forAll(gen.listOfN(3)) { t3 =>
      val (a1, a2, a3) = (t3(0), t3(1), t3(2))
      m.op(m.op(a1, a2), a3) == m.op(a1, m.op(a2, a3))
    }
    def zeroLawRight = forAll(gen) { a =>
      m.op(a, m.zero) == a
    }
    def zeroLawLeft = forAll(gen) { a =>
      m.op(m.zero, a) == a
    }
    associativityLaw && zeroLawLeft && zeroLawRight
  }

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero) { (acc, a) => m.op(acc, f(a)) }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B]) { a => f(a, _) }(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B]) { a => f(_, a) }(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = as match {
    case IndexedSeq() => m.zero
    case IndexedSeq(a) => f(a)
    case _ =>
      val (as1, as2) = as.splitAt(as.length / 2)
      val b1 = foldMapV(as1, m)(f)
      val b2 = foldMapV(as2, m)(f)
      m.op(b1, b2)
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val m = new Monoid[(Int, Boolean)] {
      override def op(m1: (Int, Boolean), m2: (Int, Boolean)): (Int, Boolean) = {
        val (a1, sortedSoFar) = m1
        if (sortedSoFar) {
          val (a2, _) = m2
          (a2, a1 <= a2)
        } else {
          (a1, false)
        }
      }

      override def zero: (Int, Boolean) = (Int.MinValue, true)
    }
    foldMapV(ints, m)(a => (a, true))._2
  }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC {
    override def toString = s"lStub: $lStub - $words - rStub: $rStub"
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(p1: Par[A], p2: Par[A]): Par[A] = Par.map2(p1, p2) { (a1, a2) => m.op(a1, a2) }

    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val pm = par(m)
    v match {
      case IndexedSeq() => pm.zero
      case IndexedSeq(a) => Par.lazyUnit(f(a))
      case _ =>
        val (as1, as2) = v.splitAt(v.length / 2)
        val b1 = parFoldMap(as1, m)(f)
        val b2 = parFoldMap(as2, m)(f)
        pm.op(b1, b2)
    }
  }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {

    override def op(wc1: WC, wc2: WC): WC = (wc1, wc2) match {
      case (Stub(cs1), Stub(cs2)) => Stub(cs1 + cs2)
      case (Stub(cs), Part(l2, c2, r2)) => Part(cs + l2, c2, r2)
      case (Part(l1, c1, r1), Stub(cs)) => Part(l1, c1, r1 + cs)
      case (Part(l1, c1, r1), Part(l2, c2, r2)) =>
        val joinedWordCount = if ((r1 + l2).isEmpty) 0 else 1
        val words = c1 + c2 + joinedWordCount
        Part(l1, words, r2)
    }

    override def zero: WC = Part("", 0, "")
  }

  def count(s: String): Int = {
    def toWC(c: Char): WC = if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)
    def wordOrNot(chars: String): SuccessCount = {
      if (chars.isEmpty) 0 else 1
    }
    foldMapV(s.toCharArray.toIndexedSeq, wcMonoid)(toWC) match {
      case Stub(chars) => wordOrNot(chars)
      case Part(left, c, right) => wordOrNot(left) + c + wordOrNot(right)
    }
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    sys.error("todo")

  // idea (hans): create maxInt Monoid and combine with booleanAnd to implement:  def ordered2(ints: IndexedSeq[Int]): Boolean = {

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    sys.error("todo")

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

trait Foldable[F[_]] {

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    sys.error("todo")

  def toList[A](as: F[A]): List[A] =
    sys.error("todo")
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

