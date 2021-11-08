package fr.brl.validation.complete

import cats.data.ValidatedNel
import cats.implicits._

import scala.util.Try

sealed abstract class ValidationErrorType(key: String, args: Any*)

case object NameMustBeginWithUppercase extends ValidationErrorType("name.must.begin.with.uppercase")

case object NameMustNotBeEmpty extends ValidationErrorType("name.must.not.be.empty")

case object AgeMustBeNumeric extends ValidationErrorType("age.must.be.numeric")

case class InvalidAge(args: Any*) extends ValidationErrorType("invalid.age.range", args: _*)

case object InvalidEmail extends ValidationErrorType("invalid.email")

case object InvalidPhone extends ValidationErrorType("invalid.phone")

final case class ValidationError(path: String, error: ValidationErrorType)

object CompleteValidation extends App {

  case class PersonForm(name: String, contacts: List[Contact], age: String)

  case class Person(name: String, contacts: List[Contact], age: Int)

  case class Contact(email: String, phone: String)

  type AllErrorsOr[T] = ValidatedNel[ValidationError, T]


  def validateName(name: String): AllErrorsOr[String] = {
    def validateUppercaseName(name: String): AllErrorsOr[String] =
      if (name.headOption.exists(_.isUpper)) name.validNel else ValidationError("person.name", NameMustBeginWithUppercase).invalidNel

    def validateNonEmptyName(name: String): AllErrorsOr[String] =
      if (name.nonEmpty) name.validNel else ValidationError("person.name", NameMustNotBeEmpty).invalidNel

    // validateNonEmptyName(name) combine validateUppercaseName(name) //accumulates all errors AND ALL SUCCESS !
    validateNonEmptyName(name) *> validateUppercaseName(name)
    // validateNonEmptyName(name) productR validateUppercaseName(name)

  }

  def validateAge(age: String): AllErrorsOr[Int] = {
    val numericAgeValidation = Try(age.toInt)
      .toEither
      .left
      .map(_ => ValidationError("person.age", AgeMustBeNumeric))
      .toValidatedNel

    def rangeAgeValidation(numericAge: Int) = {
      if (numericAge <= 0 || numericAge > 120) ValidationError("person.age", InvalidAge(0, 120)).invalidNel
      else numericAge.validNel
    }

    numericAgeValidation.andThen(rangeAgeValidation) // fails fast if numeric validation fails
  }

  def validateContacts(contacts: List[Contact]): AllErrorsOr[List[Contact]] = {

    def validateEmail(email: String, index: Int): AllErrorsOr[String] = {
      val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
      email match {
        case e if emailRegex.findFirstMatchIn(e).isDefined => e.validNel
        case _ => ValidationError(s"person.contacts.email[$index]", InvalidEmail).invalidNel
      }
    }

    def validatePhone(phone: String, index: Int): AllErrorsOr[String] = {
      val phoneRegex = """^\+(?:[0-9] ?){6,14}[0-9]$""".r
      phone match {
        case p if phoneRegex.findFirstMatchIn(p).isDefined => p.validNel
        case _ => ValidationError(s"person.contacts.phone[$index]", InvalidPhone).invalidNel
      }
    }

    /*    contacts.view
          .zipWithIndex
          .map {
            case (c, i) => (validateEmail(c.email, i), validatePhone(c.phone, i)).mapN(Contact)
          }.toList.sequence*/


    contacts.view
      .zipWithIndex
      .toList
      .traverse { case (c, i) => (validateEmail(c.email, i), validatePhone(c.phone, i)).mapN(Contact) }

  }

  def validate(p: PersonForm): AllErrorsOr[Person] = (validateName(p.name), validateContacts(p.contacts), validateAge(p.age)).mapN(Person)

  val okName = "Richard"
  val badName = "rc"
  val emptyName = ""
  val okEmail = "richard.capraro@brl.fr"
  val badEmail = "richard.capraro#brl,fr"
  val okPhone = "+33 06 77 77 77 77"
  val badPhone = "???"

  val validResult = validate(PersonForm(okName, List(Contact(okEmail, okPhone)), "47")).fold(
    f => f.toList.groupBy(v => v.path),
    identity
  )

  println(validResult)


  val invalidResult = validate(PersonForm(badName, List(Contact(okEmail, badPhone), Contact(badEmail, okPhone)), "200")).fold(
    f => f.toList.groupBy(v => v.path),
    identity
  )

  println(invalidResult)

}
