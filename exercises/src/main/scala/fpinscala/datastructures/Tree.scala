package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = {
    def go(c: Tree[A], acc: Int): Int = c match {
      case _: Leaf[A] => acc + 1
      case Branch(left, right) => go(left, acc) + go(right, acc)
    }
    go(t, 0)
  }

}