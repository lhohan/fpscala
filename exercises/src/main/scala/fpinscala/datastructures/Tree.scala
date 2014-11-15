package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case _: Leaf[A] => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case _: Leaf[A] => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](t: Tree[A])(f: Leaf[A] => B)(g: (B, B) => B): B = t match {
    case leaf: Leaf[A] => f(leaf)
    case b: Branch[A] =>
      val fold1 = fold(b.left)(f)(g)
      val fold2 = fold(b.right)(f)(g)
      g(fold1, fold2)
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(leaf => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(_.value)(_ max _)

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)((leaf: Leaf[A]) => Leaf(f(leaf.value)): Tree[B])((left, right) => Branch(left, right))

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(leaf => 0)((left, right) => 1 + (left max right))

}