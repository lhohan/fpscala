package fpinscala.parallelism

object Par2 {

  import scala.concurrent._
  import ExecutionContext.Implicits.global

  type Par[A] = ExecutionContext => Future[A]

  def unit[A](a: A): Par[A] = ec => Future.successful(a)

  def run[A](a: Par[A])(implicit ec: ExecutionContext): Future[A] = a(ec)

  def fork[A](a: => Par[A]): Par[A] = implicit ec => Future(a).flatMap(_.run)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    pa.map2(unit(()))((a, _) => f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A]))((p, acc) => p.map2(acc)(_ :: _))

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = { implicit ec =>
    val fa: Future[A] = pa(ec)
    fa.flatMap(f(_)(ec))
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pas: List[Par[List[A]]] = as.map {
      asyncF(a => if (f(a)) List(a) else List())
    }
    val pasl: Par[List[List[A]]] = sequence(pas)
    pasl.map(_.flatten)
  }

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = parList.map(_.sorted)

  def parFilter3[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val xs: List[Par[Option[A]]] = as.map { a => lazyUnit(if (f(a)) Some(a) else None) }
    sequence(xs).map(_.flatten)
  }

  def parFilter2[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pas: List[Par[List[A]]] = as.map {
      asyncF(a => if (f(a)) List(a) else List())
    }
    val pasl: Par[List[List[A]]] = sequence(pas)
    pasl.map(_.flatten)
  }

  def parFilterB[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    parMap(as)(a => if (f(a)) Some(a) else None).map(_.flatten)
  }

  /* Gives us infix syntax for `Par`. */
  //implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  // infix versions of `map`, `map2`
  implicit class ParOps[A](a: Par[A]) {
    //def map[B](f: A => B): Par[B] = map2(unit(()))((x, _) => f(x))
    def map[B](f: A => B): Par[B] = { implicit ec =>
      a.run.map(f)
    }

    def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = { implicit ec =>
      val fa = a.run
      val fb = b.run
      for {
        a1 <- fa
        b1 <- fb
      } yield {
        f(a1, b1)
      }
    }

    def map3[B, C, D](b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = { implicit ec =>
      val x: Par[C => D] = a.map2(b) { (a_, b_) => f(a_, b_, _) }
      x.map2(c)((fcd, d) => fcd(d)).run
    }

    //def zip[B](b: Par[B]): Par[(A,B)] = p.map2(b)((_,_))
    def run(implicit ec: ExecutionContext): Future[A] = a(ec)

  }

}

object Examples2 {

  import Par2._

  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1) {
      ints.headOption getOrElse 0
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }

  def parSum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1) {
      lazyUnit(ints.headOption getOrElse 0)
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val pl: Par[Int] = fork(parSum(l))
      val pr: Par[Int] = fork(parSum(r))
      pl.map2(pr) { (a, b) => println(s"summing $a and $b"); a + b }
    }

  def parExec[A, B](z: => B)(f: A => B)(as: IndexedSeq[A])(comb: (B, B) => B): Par[B] =
    if (as.size <= 1) {
      lazyUnit(as.map(f).headOption getOrElse z)
    } else {
      val (l, r) = as.splitAt(as.length / 2)
      val pl: Par[B] = fork(parExec(z)(f)(l)(comb))
      val pr: Par[B] = fork(parExec(z)(f)(r)(comb))
      pl.map2(pr) { (a, b) => println(s"exeuting $a and $b"); comb(a, b) }
    }

  type Paragraph = String
  def wordCount(ps: List[Paragraph]): Par[Option[Int]] = {
    val z = None: Option[Int]
    val f = (p: Paragraph) => Option(p.split(" ").length)
    val combine: (Option[Int], Option[Int]) => Option[Int] = (ma, mb) => ma.flatMap(a => mb.map(b => a + b))
    parExec(z)(f)(ps.toVector)(combine)
  }

  def parSumExec(ints: IndexedSeq[Int]): Par[Int] = parExec(0)((i: Int) => i)(ints)(_ + _)

  def parMaxExec(ints: IndexedSeq[Int]): Par[Option[Int]] = {
    val z = None: Option[Int]
    val f = (i: Int) => Option(i)
    val combine: (Option[Int], Option[Int]) => Option[Int] = (ma, mb) => ma.flatMap(a => mb.map(b => a max b))
    parExec(z)(f)(ints)(combine)
  }

}
