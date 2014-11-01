package fpinscala.laziness

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
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
    case Cons(h, t) => cons(h(), t().take(n - 1))
    case _ => empty[A]
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n == 0 => this
    case Cons(h, t) => t().drop(n - 1)
    case Empty => empty[A]
  }

  // tail recursive version
  def drop2(n: Int): Stream[A] = {
    @tailrec
    def loop(as: Stream[A], i: Int): Stream[A] = {
      if (i <= 0) as
      else {
        as match {
          case Cons(h, t) => loop(t(), i - 1)
          case Empty => empty[A]
        }
      }
    }
    loop(this, n)
  }


  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty[A]
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true) { (el, acc) => acc && p(el)}

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
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

  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}