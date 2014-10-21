package fpinscala.datastructures

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("tail not supported on empty list")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("setHead not supported on empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) {
      l
    }
    else l match {
      case Nil => throw new UnsupportedOperationException("drop not supported on empty list")
      case Cons(_, xs) => drop(xs, n - 1)
    }

  //tail in terms of drop
  def tail2[A](l: List[A]): List[A] = drop(l, 1)


  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, xs) if f(h) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("init on empty list")
    case Cons(h, Nil) => Nil
    case Cons(h, xs) => Cons(h, init(xs))
  }

  def init2[A](l: List[A]): List[A] = {
    @tailrec
    def go(acc: List[A], rest: List[A]): List[A] = rest match {
      case Nil => throw new UnsupportedOperationException("init2 on empty list")
      case Cons(_, Nil) => acc
      case Cons(h, xs) => go(Cons(h, acc), xs)
    }
    rev(go(Nil, l))
  }

  def rev[A](l: List[A]): List[A] = {
    @tailrec
    def go(acc: List[A], l: List[A]): List[A] = l match {
      case Nil => acc
      case Cons(h, xs) => go(Cons(h, acc), xs)
    }
    go(Nil, l)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, xs) => foldLeft(xs, f(z, h))(f)
  }

  def sumLF(is: List[Int]): Int = foldLeft(is, 0)(_ + _)

  def productLF(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def reverse[A](ls: List[A]): List[A] = foldLeft(ls, List[A]()) { (acc, el) => Cons(el, acc)}

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldLeft(reverse(l), z)(f)

  def appendRF[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2) { (el, acc) => Cons(el, acc)}

  def appendLF[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2) { (acc, el) => Cons(el, acc)}

  def concat[A](ls: List[List[A]]): List[A] = foldRight(ls, List[A]())(append)

  def add1(l: List[Int]): List[Int] = foldRight(l, List[Int]()) { (el, acc) => Cons(el + 1, acc)}

  def doubleToString(l: List[Double]): List[String] = foldRight(l, List[String]()) { (el, acc) => Cons(el.toString, acc)}

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]()) { (el, acc) => Cons(f(el), acc)}

  def map_tr[A, B](l: List[A])(f: A => B): List[B] = {
    val lb = new ListBuffer[B]
    @tailrec
    def loop(cur: List[A]): Unit = cur match {
      case Nil => ()
      case Cons(h, xs) =>
        lb.append(f(h))
        loop(xs)
    }
    loop(l)
    List(lb: _*)
  }

  def filter[A](as: List[A])(f: A => Boolean) = {
    val lb = new ListBuffer[A]
    @tailrec
    def loop(cur: List[A]): Unit = cur match {
      case Nil => ()
      case Cons(h, xs) =>
        if (f(h)) lb.append(h)
        loop(xs)
    }
    loop(as)
    List(lb: _*)
  }

  // not stack safe, a bit coarse too
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(foldRight(as, List[List[B]]()) { (el, acc) => Cons(f(el), acc)})
  }

  // stack safe, short but don't like double traversal
  def flatMap_2[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))
}