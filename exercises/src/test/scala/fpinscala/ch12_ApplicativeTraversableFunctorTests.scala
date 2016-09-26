package fpinscala

import java.util.{Calendar, Date}

import fpinscala.applicative.Applicative._
import fpinscala.applicative.Monad._
import fpinscala.applicative.{Failure, Success, Validation}
import org.scalatest.FunSuite

class ch12_ApplicativeTraversableFunctorTests extends FunSuite {

  test("12.4") {

    val sequenced = streamApplicative.sequence(List(Stream(1, 2, 3), Stream(4, 5, 6, 7)))

    assert(List(List(1, 4), List(2, 5), List(3, 6)) == sequenced)
    // so:
    /*
    1 2 3
    4 5 6
    becomes
    1 4
    2 5
    3 6
    so sequencing means 'transposition of a matrix'
     */

  }

  test("12.5") {
    assert(Right(6) == eitherMonad.map(Right(5))(_ + 1))
    assert(Left(5) == eitherMonad.map(Left[Int, Int](5))(_ + 1))
  }

  test("12.6") {
    case class WebForm(name: String, birthdate: String, phoneNumber: Int)

    def validName(name: String): Validation[String, String] =
      if (name != "") Success(name)
      else Failure("Name cannot be empty")

    def validBirthdate(birthdate: String): Validation[String, String] =
      try {
        import java.text._
        new SimpleDateFormat("yyyy-MM-dd").parse(birthdate)
        Success(birthdate)
      } catch {
        case e: Throwable => Failure("Birthdate must be in the form yyyy-MM-dd")
      }

    def validPhone(phoneNumber: String): Validation[String, Int] =
      if (phoneNumber.matches("[0-9]{10}"))
        Success(phoneNumber.toInt)
      else Failure("Phone number must be 10 digits")

    def validWebForm(name: String, birthdate: String, phone: String): Validation[String, WebForm] =
      validationApplicative.map3(
        validName(name),
        validBirthdate(birthdate),
        validPhone(phone)
      )(WebForm(_, _, _))

    assert(Success(WebForm("Dale", "2015-12-22", 1234567890)) == validWebForm("Dale", "2015-12-22", "1234567890"))
    assert(Failure("Name cannot be empty", Vector("Birthdate must be in the form yyyy-MM-dd", "Phone number must be 10 digits")) == validWebForm("", "invlaid date", "123456789"))

  }
}
