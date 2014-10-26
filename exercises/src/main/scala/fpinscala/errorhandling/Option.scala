package fpinscala.errorhandling


import scala.{Either => _, Option => _, Some => _}

// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  //  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
  //    case None => ob
  //    case _ => this
  //  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse (ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) this else None)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m =>
      mean(xs.map(x => math.pow(x - m, 2)))
    )
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap { a_ =>
    b.map(b_ => f(a_, b_))
  }

  // my original
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(List())
    case ao :: as => map2(ao, sequence(as))(_ :: _)
  }

  def sequence_2[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(List())
    case ao :: as => map2(ao, sequence(as))(_ :: _)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sys.error("todo")
}