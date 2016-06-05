package fpinscala.parsing

import scala.language.higherKinds

// import our own Either type
import fpinscala.errorhandling.Either

import scala.language.implicitConversions

trait Parsers[Parser[+_]] {
  self =>
  // so inner classes may call methods of trait

  def run[A](p: Parser[A])(s: String): Either[ParseError, A]

  def char(c: Char): Parser[Char]

  def strings(s: String): Parser[String]

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def succeeded[A](a: A): Parser[A] = strings(" ") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String] = ???

  def product[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] = ???

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = ???

    // replace later by flatMap when we have it
    def map[B](f: A => B): Parser[B] = map2(char(' ')) { case (a, _) => f(a) }

    def map2[B, C](pb: => Parser[B])(f: (A, B) => C): Parser[C] =
      self.product(p, pb).map { case (a, b) => f(a, b) }

    def many1: Parser[List[A]] = map2(many) { case (a, as) => a :: as }

    def product[B](pb: => Parser[B]): Parser[(A, B)] = self.product(p, pb)

    def **[B](pb: Parser[B]): Parser[(A, B)] = self.product(p, pb)

  }

  object Laws {

    import fpinscala.testing.Prop.forAll
    import fpinscala.testing.{Prop, _}

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = {
      forAll(in) { s => run(p1)(s) == run(p2)(s) }
    }

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = {
      equal(p, p.map(a => a))(in)
    }

    // could also be passing extra Gen[A], better?
    def succeedLaw[A](a: A, p: Parser[A])(in: Gen[String]): Prop = {
      forAll(in) { s => run(succeeded(a))(s) == a }
    }

    // we could map on the Parser directly to swap A and B but that would make
    // this test also need and test map, below we keep it strictly product test
    def productLaw1[A, B](pa: Parser[A], pb: Parser[B])(in: Gen[String]): Prop = {
      forAll(in) { s =>
        val productab: Parser[(A, B)] = product(pa, pb)
        val productba: Parser[(B, A)] = product(pb, pa)

        run(productab)(s) == run(productba)(s).map(_.swap) // swap in value in Right is exists
      }
    }

    // uses map, so less pure test of product, still useful?
    def productLaw2[A, B](pa: Parser[A], pb: Parser[B])(in: Gen[String]): Prop = {
      val productab: Parser[(A, B)] = product(pa, pb)
      val productba: Parser[(A, B)] = product(pb, pa).map(_.swap)
      equal(productab, productba)(in)
    }

    // is one parser always succeed then the result of the product is the result of the 2nd parser
    def productLaw3[A, B](pa: Parser[A], a: A)(in: Gen[String]): Prop = {
      val succ = succeeded(a)
      val prod: Parser[(A, A)] = product(succ, pa)
      equal(pa, prod)(in)
    }

  }

}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

case class ParseError(
    stack: List[(Location, String)] = List(),
    otherFailures: List[ParseError] = List()
) {
}