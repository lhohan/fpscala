package fpinscala.localeffects

import fpinscala.monads._

import scala.collection.mutable

object Mutable {
  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs
    else {
      val arr = xs.toArray

      def swap(x: Int, y: Int) = {
        val tmp = arr(x)
        arr(x) = arr(y)
        arr(y) = tmp
      }

      def partition(l: Int, r: Int, pivot: Int) = {
        val pivotVal = arr(pivot)
        swap(pivot, r)
        var j = l
        for (i <- l until r) if (arr(i) < pivotVal) {
          swap(i, j)
          j += 1
        }
        swap(j, r)
        j
      }

      def qs(l: Int, r: Int): Unit = if (l < r) {
        val pi = partition(l, r, l + (r - l) / 2)
        qs(l, pi - 1)
        qs(pi + 1, r)
      }

      qs(0, arr.length - 1)
      arr.toList
    }
}

sealed trait ST[S, A] { self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S, A](a: => A) = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A =
    st[Null].run(null)._1
}

sealed trait STRef[S, A] {
  protected var cell: A

  def read: ST[S, A] = ST(cell)

  def write(a: => A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] =
    ST(new STRef[S, A] {
      var cell = a
    })
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

// Scala requires an implicit Manifest for constructing arrays.
sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.size)

  // Write a value at the give index of the array
  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      value(i) = a
      ((), s)
    }
  }

  // Read the value at the given index of the array
  def read(i: Int): ST[S, A] = ST(value(i))

  // Turn the array into an immutable list
  def freeze: ST[S, List[A]] = ST(value.toList)

  def fill(xs: Map[Int, A]): ST[S, Unit] = ST {
    xs.foreach { case (i, v) => value(i) = v }
    value
  }

  def swap(i: Int, j: Int): ST[S, Unit] =
    for {
      x <- read(i)
      y <- read(j)
      _ <- write(i, y)
      _ <- write(j, x)
    } yield ()

}

object STArray {
  // Construct an array of the given size filled with the value v
  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value = Array.fill(sz)(v)
    })

  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value = xs.toArray
    })
}

object Immutable {
  def noop[S] = ST[S, Unit](())

  def partition[S](a: STArray[S, Int], n: Int, r: Int, pivot: Int): ST[S, Int] = {
    for {
      pivotValue <- a.read(pivot)
      _          <- a.swap(pivot, r)
      j          <- STRef(n)
      _ <- (n until r).foldLeft(noop[S]) { (s, i) =>
        for {
          _  <- s
          ai <- a.read(i)
          _ <- if (ai < pivotValue) {
            for {
              jValue <- j.read
              _      <- a.swap(i, jValue)
              _      <- j.write(jValue + 1)
            } yield ()
          } else noop[S]

        } yield ()
      }
      jValue <- j.read
      _      <- a.swap(jValue, r)
    } yield jValue
  }

  def qs[S](a: STArray[S, Int], n: Int, r: Int): ST[S, Unit] = {
    if (n < r) {
      for {
        pi <- partition(a, n, r, n + (r - n) / 2)
        _  <- qs(a, n, pi - 1)
        _  <- qs(a, pi + 1, r)
      } yield ()
    } else noop[S]
  }

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs
    else
      ST.runST(new RunnableST[List[Int]] {
        def apply[S] =
          for {
            arr    <- STArray.fromList(xs)
            size   <- arr.size
            _      <- qs(arr, 0, size - 1)
            sorted <- arr.freeze
          } yield sorted
      })
}

// Scala requires an implicit Manifest for constructing arrays.
sealed abstract class STHashMap[S, K, V] {
  protected def value: mutable.HashMap[K, V]

  def size: ST[S, Int] = ST(value.size)

  // Put a value at the give key of the map
  def put(k: K, v: V): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      value.put(k, v)
      ((), s)
    }
  }

  // Read the value at the given key of the map
  def get(k: K): ST[S, V] = ST(value(k))

  def freeze: ST[S, Map[K, V]] = ST(value.toMap)

  def keys: ST[S, Set[K]] = ST(value.keySet.toSet)
}

object STHashMap {
  def apply[S, K, V](kv: (K, V)*): ST[S, STHashMap[S, K, V]] =
    ST(new STHashMap[S, K, V] {
      lazy val value = {
        val x = new mutable.HashMap[K, V]()
        kv.foreach { case (k, v) => x.put(k, v) }
        x
      }
    })
}
