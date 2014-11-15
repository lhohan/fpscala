package fpinscala.state

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, r) = rng.nextInt
    val abs_v = if (v < 0) -(v + 1) else v
    (abs_v, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (v, r) = nonNegativeInt(rng)
    (v / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (id, r) = intDouble(rng)
    (id.swap, r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  //  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  //    (0 to count-1).foldRight((List.empty[Int], rng)){(el, acc) =>
  //      val (is, r) = acc
  //      val (ni, r1) = r.nextInt
  //      (ni :: is, r1)
  //    }
  //  }

  // tail rec version
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(count: Int, rng: RNG, is: List[Int]): (List[Int], RNG) = count match {
      case c if c == 0 => (is, rng)
      case _ =>
        val (next_i, next_r) = rng.nextInt
        loop(count - 1, next_r, next_i :: is)
    }
    loop(count, rng, List())
  }

  // ex. 6.5
  def doubleRnd = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def doubleElegant(rng: RNG): (Double, RNG) = doubleRnd(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rnd => {
      val (a, ra1) = ra(rnd)
      val (b, rb1) = rb(ra1)
      (f(a, b), rb1)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A])) { (r, acc) => map2(r, acc)(_ :: _) }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
