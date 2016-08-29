package fpinscala
package monads

import fpinscala.parallelism.Nonblocking._
import fpinscala.parsing._
import fpinscala.state._
import fpinscala.testing._

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List.empty[A])) { (ma, acc) => map2(ma, acc)(_ :: _) }

  def sequence_[A](lma: List[M[A]]): M[List[A]] =
    traverse(lma)(ma => ma)

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List.empty[B])) { (a, acc) => map2(f(a), acc)(_ :: _) }

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = sequence(List.fill(n)(ma))

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldRight(unit(List.empty[A])) { (a, acc) => map2(f(a), acc) { (b, as) => if (b) a :: as else as } }
  //    ms match {
  //      case Nil => unit(List.empty[A])
  //      case a :: as =>
  //        val mb = f(a)
  //        map2(mb, filterM(as)(f))((b, xs) => if (b) a :: xs else xs)
  //    }

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = {
    val g: Unit => M[A] = _ => ma
    val c1: (Unit) => M[B] = compose(g, f)
    c1(())
  }

  //exercise 11.9
  /*
  compose(compose(f, g), h) == compose(f, compose(g, h))
  compose(a => flatMap(f(a))(g), h) == compose(f, a => flatMap(g(a))(h))
  b => flatMap((a => flatMap(f(a))(g)))(h) == b => flatMap(f(b))(a => flatMap(g(a))(h)))
  b => flatMap(flatMap(f(b)(g)))(h) == b => flatMap(f(b))(a => flatMap(g(a))(h)))

  apply to x:

  flatMap(flatMap(f(x)(g)))(h) == flatMap(f(x))(a => flatMap(g(a))(h)))

  subst f(x) = y or just x again:

  flatMap(flatMap(x)(g))(h) == flatMap(x)(a => flatMap(g(a))(h))

  infix notation:

  x.flatMap(g).flatMap(h) == x.flatMap(a => g(a).flatMap(h))
   */

  def join[A](mma: M[M[A]]): M[A] = ???

  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = ???
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] = {
      Par.flatMap(ma)(f)
    }

    override def unit[A](a: => A): Par[A] = {
      Par.unit(a)
    }
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    override def flatMap[A, B](ma: P[A])(f: (A) => P[B]): P[B] = {
      p.flatMap(ma)(f)
    }

    override def unit[A](a: => A): P[A] = p.succeed(a)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = {
      ma.flatMap(f)
    }

    override def unit[A](a: => A): Option[A] = Option.apply(a)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = {
      ma.flatMap(f)
    }

    override def unit[A](a: => A): Stream[A] = Stream.apply(a)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = {
      ma.flatMap(f)
    }

    override def unit[A](a: => A): List[A] = {
      List(a)
    }
  }

  // ex11.2: attempt 1 : OK
  //  type MyState[A] = State[Int, A]
  //  def stateMonad[A]: Monad[MyState] = new Monad[MyState] {
  //    override def unit[A](a: => A): MyState[A] = {
  //      State(s => (a, s))
  //    }
  //
  //    override def flatMap[A, B](ma: MyState[A])(f: (A) => MyState[B]): MyState[B] = {
  //      ma.flatMap(f)
  //    }
  //  }

  // ex11.2: attempt 2: NOK
  //  def stateMonad2[A]: Monad[State[Int, A]] = new Monad[State[Int, A]] {
  //    override def unit[A](a: => A): State[Int, A] = {
  //      State(s => (a, s))
  //    }
  //
  //    override def flatMap[A, B](ma: State[Int, A])(f: (A) => State[Int, B]): State[Int, B] = {
  //      ma.flatMap(f)
  //    }
  //  }

  // ex11.2: attempt 3: Compiles
  //  type MyState[A] = State[Int, A]
  //  type t1[A] = ({ type MyState[A] = State[Int, A] })#MyState[A]
  //  def stateMonad[A]: Monad[t1] = new Monad[t1] {
  //    override def unit[A](a: => A): t1[A] = {
  //      State(s => (a, s))
  //    }
  //
  //    override def flatMap[A, B](ma: t1[A])(f: (A) => t1[B]): t1[B] = {
  //      ma.flatMap(f)
  //    }
  //  }

  // ex11.2: attempt 4
  //  type MyState[A] = State[Int, A]
  //  type t1[A] = ({ type MyState[A] = State[Int, A] })#MyState[A]
  // parametrizing t1 gives us something like:
  type t2[A, S] = ({ type MyState[A] = State[S, A] })#MyState[A]
  // The above seems to partially apply a type, so removing it and putting it in the Monad works:
  def stateMonad[S] = new Monad[({ type MyState[A] = State[S, A] })#MyState] {
    // I could not refere to MyState in the method definitions?
    override def unit[A](a: => A): State[S, A] = {
      State(s => (a, s))
    }

    override def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] = {
      ma.flatMap(f)
    }
  }

  //  val idMonad: Monad[Id] = ???

  def readerMonad[R] = ???
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = ???
  def flatMap[B](f: A => Id[B]): Id[B] = ???
}

object Reader {
  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    def unit[A](a: => A): Reader[R, A] = ???
    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = ???
  }
}

