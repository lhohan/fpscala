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

}