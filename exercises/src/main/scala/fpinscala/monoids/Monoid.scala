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

  def monoidLawsFunction1[A](maa: Monoid[A => A], gen: Gen[A], ma: Monoid[A]): Prop = {

    def fGen = gen.map(a => (a0: A) => ma.op(a, a0))

    val associativityLaw = forAll(gen ** fGen.listOfN(3)) {
      case (arg, t3) =>
        val (f1, f2, f3) = (t3(0), t3(1), t3(2))
        maa.op(maa.op(f1, f2), f3)(arg) == maa.op(f1, maa.op(f2, f3))(arg)
    }
    def zeroLawRight = forAll(gen ** fGen) {
      case (arg, f) =>
        maa.op(f, maa.zero)(arg) == f(arg)
    }
    def zeroLawLeft = forAll(gen ** fGen) {
      case (arg, f) =>
        maa.op(maa.zero, f)(arg) == f(arg)
    }

    associativityLaw && zeroLawLeft && zeroLawRight
  }

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero) { (acc, a) => m.op(acc, f(a)) }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B]) { a => f(a, _) }(z)

  def dual[A](m: Monoid[A]) = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)

    override def zero: A = m.zero
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B])) { a => f(_, a) }(z)

  //  def dualMonoid[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
  //    def op(a1: A, a2: A) = m.op(a2, a1)
  //    val zero = m.zero
  //  }
  //def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
  //foldMap(as, dualMonoid(endoMonoid[B]))(a => b => f(b, a))(z)

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

    type Range = (Int, Int)
    type IsOrdered = Boolean
    case class Ordered(range: Option[Range], isOrdered: IsOrdered)

    val m = new Monoid[Ordered] {
      override def op(s1: Ordered, s2: Ordered): Ordered = (s1, s2) match {
        case (Ordered(Some((l1, r1)), true), Ordered(Some((l2, r2)), true)) => Ordered(Some(l1, r2), r1 <= l2)
        case (Ordered(None, true), x @ Ordered(sr, true)) => x
        case (x @ Ordered(sr, true), Ordered(None, true)) => x
        case (x @ Ordered(None, true), Ordered(None, true)) => x
        case _ => Ordered(None, false)
      }

      override def zero = Ordered(None, true)
    }

    val result = foldMapV(ints, m)(i => Ordered(Some((i, i)), true))
    result.isOrdered
  }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC {
    override def toString = s"lStub: $lStub - $words - rStub: $rStub"
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(p1: Par[A], p2: Par[A]): Par[A] = Par.map2(p1, p2)(m.op)

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
    def toWC(c: Char): WC = if (c.isWhitespace) wcMonoid.zero else Stub(c.toString)
    def wordOrNot(chars: String): Int = {
      if (chars.isEmpty) 0 else 1
    }
    foldMapV(s.toCharArray.toIndexedSeq, wcMonoid)(toWC) match {
      case Stub(chars) => wordOrNot(chars)
      case Part(left, c, right) => wordOrNot(left) + c + wordOrNot(right)
    }
  }

  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {

      override def op(ab1: (A, B), ab2: (A, B)): (A, B) = {
        val (a1, b1) = ab1
        val (a2, b2) = ab2
        (ma.op(a1, a2), mb.op(b1, b2))
      }

      override def zero: (A, B) = (ma.zero, mb.zero)
    }

  // idea (hans): create maxInt Monoid and combine with booleanAnd to implement:
  // START
  val maxIntMonoid = new Monoid[Option[Int]] {
    override def op(a1: Option[Int], a2: Option[Int]): Option[Int] = (a1, a2) match {
      case (Some(i1), Some(i2)) => Some(i1 max i2)
      case (Some(i1), None) => Some(i1)
      case (None, Some(i2)) => Some(i2)
      case (None, None) => None
    }

    override def zero: Option[Int] = None
  }

  def ordered2(ints: IndexedSeq[Int]): Boolean = foldMapV(ints, productMonoid(maxIntMonoid, booleanAnd)) {
    i => (Some(i), true)
  }._2

  // END: will always be true. Conclusion (?): product are independent types while here we want the boolean part to
  // be dependent on the max part. Looks like we need flatMap.?

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[(A) => B] {
      override def op(a1: (A) => B, a2: (A) => B): (A) => B = (a: A) => B.op(a1(a), a2(a))

      override def zero: (A) => B = (a: A) => B.zero
    }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()

      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(
            a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)
          ))
        }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    IndexedSeqFoldable.foldMap(as)((a: A) => Map(a -> 1))(mapMergeMonoid(intAddition))
}

trait Foldable[F[_]] {

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as) { a => b: B => f(a, b) }(Monoid.endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as) { a => b: B => f(b, a) }(Monoid.dual(Monoid.endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((acc, a) => mb.op(acc, f(a)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldLeft(as)(List.empty[A])((acc, a) => acc :+ a)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
    case Leaf(a) => f(z, a)
    case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
    case Leaf(a) => f(a, z)
    case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    as.map(a => f(z, a)).getOrElse(z)

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    as.map(a => f(a, z)).getOrElse(z)

}

