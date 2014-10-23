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

  def depth[A](t: Tree[A]): Int = {
    def depth(x: Tree[A], count: Int): Int = x match {
      case _: Leaf[A] => count + 1
      case Branch(left, right) => depth(left, count + 1) max depth(right, count + 1)
    }
    depth(t, 0)
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

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)((leaf: Leaf[A]) => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int = fold(t)(_.value)(_ max _)

}