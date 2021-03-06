package fpinscala.laziness

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  //  def toList: List[A] = foldRight(List.empty[A])((el, acc) => el :: acc)

  // below I wrote to avoid having to reverse:
  //  def toList: List[A] = {
  //    @tailrec
  //    def loop(s: Stream[A], acc: List[A]): List[A] =
  //      s match {
  //        case Empty => acc
  //        case Cons(h, t) => loop(t(), acc.++(List(h())))
  //      }
  //    loop(this, List[A]())
  //  }

  def toList: List[A] = {
    val lb = new ListBuffer[A]
    def loop(s: Stream[A]): ListBuffer[A] = s match {
      case Empty => lb
      case Cons(h, t) =>
        lb.append(h())
        loop(t())
    }
    loop(this).toList
  }

  import fpinscala.laziness.Stream._

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n == 1 => cons(h(), empty[A])
    case Cons(h, t)           => cons(h(), t().take(n - 1))
    case _                    => empty[A]
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n == 0 => this
    case Cons(h, t)           => t().drop(n - 1)
    case Empty                => empty[A]
  }

  // tail recursive version
  def drop2(n: Int): Stream[A] = {
    @tailrec
    def loop(as: Stream[A], i: Int): Stream[A] = {
      if (i <= 0) as
      else {
        as match {
          case Cons(h, t) => loop(t(), i - 1)
          case Empty      => empty[A]
        }
      }
    }
    loop(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => empty[A]
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true) { (el, acc) =>
    acc && p(el)
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A]) { (el, acc) =>
    if (p(el)) cons(el, acc)
    else acc
  }

  def headOption: Option[A] = foldRight(None: Option[A])((el, _) => Some(el))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B]) { (el, acc) =>
    cons(f(el), acc)
  }

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A]) { (el, acc) =>
    if (p(el)) cons(el, acc) else acc
  }

  def append[B >: A](bs: => Stream[B]): Stream[B] = this.foldRight(bs) { (el, acc) =>
    cons(el, acc)
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B]) { (el, acc) =>
    f(el) append acc
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case Empty      => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Empty, _)       => None
    case (_, i) if i <= 0 => None
    case (Cons(h, t), i)  => Some((h(), (t(), i - 1)))
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Empty => None
    case Cons(h, t) =>
      lazy val h_ = h()
      if (p(h_)) Some((h(), t()))
      else None
  }

  // special case of `zip`
  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _                            => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (_, Cons(h, t))              => Some((None, Some(h())), (empty, t()))
    case (Cons(h, t), _)              => Some((Some(h()), None), (t(), empty))
    case _                            => None
  }

  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).forAll {
    case (Some(_), None)      => true
    case (Some(x1), Some(x2)) => x1 == x2
    case _                    => false
  }

  def tails: Stream[Stream[A]] = unfold((this, true)) {
    case (c @ Cons(h, t), _) => Some((c, (t(), true)))
    case (Empty, true)       => Some(Empty, (Stream(), false))
    case _                   => None
  }

  def hasSubSequence[B](sub: Stream[B]) = tails.exists(_.startsWith(sub))

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibsInit(a: Int, b: Int): Stream[Int] = Stream.cons(a, fibsInit(b, a + b))

  val fibs = fibsInit(0, 1)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None         => empty
    }

  val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(x => Some((x, x + 1)))

  def fibsViaUnfold: Stream[Int] = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

}
