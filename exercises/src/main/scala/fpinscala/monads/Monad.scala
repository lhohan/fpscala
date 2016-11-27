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
    case Left(fa)  => map(fa)(Left(_))
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
    lma.foldRight(unit(List.empty[A])) { (ma, acc) =>
      map2(ma, acc)(_ :: _)
    }

  def sequence_[A](lma: List[M[A]]): M[List[A]] =
    traverse(lma)(ma => ma)

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List.empty[B])) { (a, acc) =>
      map2(f(a), acc)(_ :: _)
    }

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = sequence(List.fill(n)(ma))

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldRight(unit(List.empty[A])) { (a, acc) =>
      map2(f(a), acc) { (b, as) =>
        if (b) a :: as else as
      }
    }
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
    val g: Unit => M[A]    = _ => ma
    val c1: (Unit) => M[B] = compose(g, f)
    c1(())
  }

  //exercise 11.9
  /*

  Note: interesting observation one could consider going back between Kleisli arrows and 'standard' Monad laws: since
  the arrows are functions and you want to go to the Monad representation, you will probably have to apply these
   functions (to general arguments).

  compose(compose(f, g), h) == compose(f, compose(g, h))
  compose(a => flatMap(f(a))(g), h) == compose(f, a => flatMap(g(a))(h))
  (b => flatMap((a => flatMap(f(a))(g))))(b)(h) == b => flatMap(f(b))(a => flatMap(g(a))(h)))
  apply both sides to y
  flatMap((a => flatMap(f(a))(g)))(y)(h) == flatMap(f(y))(a => flatMap(g(a))(h))
  applying left side `a => ` arrow using `y`
  flatMap(flatMap(f(y)(g)))(h) == flatMap(f(y))(a => flatMap(g(a))(h)))

  subst f(y) = x :

  flatMap(flatMap(x)(g))(h) == flatMap(x)(a => flatMap(g(a))(h))

  infix notation:

  x.flatMap(g).flatMap(h) == x.flatMap(a => g(a).flatMap(h))
   */

  // ex. 11.10
  /*
  left:
  -----

  compose(f, unit) == f
  a => flatMap(f(a))(unit) == f

  apply to v:

  flatMap(f(v))(unit) = f(v)

  subst f(v) = x:

  flatMap(x)(unit) == x
  q.e.d

  right:
  ------

  compose(unit, f) == f
  a => flatMap(unit(a))(f) == f

  apply to v:

  flatMap(unit(v))(f) = f(v)
  q.e.d.
   */

  // ex. 11.11
  /*
  Option:
  left
  -----
  flatMap(None)(f) == None
  Trivial

  flatMap(Some(v))(unit) == Some(v)
  unit(v) == Some(v)
  Some(v) == Some(v)
  q.e.d

  right:
  ------
  flatMap(unit(None))(f) == f(None)
  flatMap(Some(None))(f) == f(None)
  f(None) == f(None)

  q.e.d.

  flatMap(unit(Some(v)))(f) == f(Some(v))
  Same as for None
  q.e.d
   */

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)

  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

  // ex. 11.14
  /*
  Associative law:

   x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
   flatMap(flatMap(x)(f))(g) == flatMap(x)(a => f(a).flatMap(g))
   join(map(join(map(x)(f))(g)) == join(map(x)(a => join(map(f(a))(g))))
   map(join(map(x)(f))(g) == map(x)(a => join(map(f(a))(g)))

  Should become (https://github.com/fpinscala/fpinscala/wiki/Chapter-11:-Monads):
  join(join(x)) == join(map(x)(join))

 Identity laws:
   flatMap(x)(unit) == x
   join(map(x))(unit) == x


   flatMap(unit(y))(f) == f(y)
   join(map(unit(y)))(f) == f(y)

   */
  // ex. 11.15
  /*
  assoc. law for Par:
  x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
  compose(compose(f, g), h) == compose(f, compose(g, h))
  join(join(x)) == join(map(x)(join))

  Considering x of type Par[Par[Par[A]]]:
  Meaning of `join` for `Par`: `join` starts an inner computation, waits for it to finish and return the result.
  So: associativity law for parallel computations: it does not matter if the outer computation is started first and then
  the inner one or vice versa, the end result should be the same.


  Considering x of type Parser[Parser[Parser[A]]]:
  x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))

  Meaning of `flatMap` for `Parser`: 'runs' a parser and then uses its result to select a second parser to run in sequence.
  So: associativity law for parsing: given 2 'parser selection functions', f and g, it should not matter if we first
  apply f and then g or vice versa, the result should be the same.

 */
}

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

  def parserMonad[P[+ _]](p: Parsers[P]): Monad[P] = new Monad[P] {
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
    // I could not refer to MyState in the method definitions?
    override def unit[A](a: => A): State[S, A] = {
      State(s => (a, s))
    }

    override def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] = {
      ma.flatMap(f)
    }
  }

  def getState[S]: State[S, S]          = State(s => (s, s))
  def setState[S](s: S): State[S, Unit] = State(_ => ((), s))

  val idMonad: Monad[Id] = new Monad[Id] {
    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma.flatMap(f)

    override def unit[A](a: => A): Id[A] = Id(a)
  }

  def readerMonad[R] = Reader.readerMonad[R]
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B]         = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

case class Reader[R, A](run: R => A) {

  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = Reader { r =>
    val a = run(r)
    f(a).run(r)
  }

  // map is function composition
  def map[B](f: A => B): Reader[R, B] = Reader { r =>
    run.andThen(f)(r)
  }

}

object Reader {
  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      st.flatMap(f)

    def get  = Reader[R, R](r => r) // like State monad but simpler
    def read = get
  }

  // join is curried functional application with the same argument twice
  def join[R, A](x: Reader[R, Reader[R, A]]): Reader[R, A] = Reader { r =>
    x.run(r).run(r)
  }
}
