package fr.brl.validation.complete

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import cats.implicits._

import scala.util.Try

trait ValidationErrorType {
  def key: String
  def args: List[Any] = List.empty
}

case object NameMustBeginWithUppercase extends ValidationErrorType() {
  def key = "name.must.begin.with.uppercase"
}

case object NameMustNotBeEmpty extends ValidationErrorType{
  def key = "name.must.not.be.empty"
}

case object AgeMustBeNumeric extends ValidationErrorType {
  def key = "age.must.be.numeric"
}

case class InvalidAge(params: Any*) extends ValidationErrorType {
  def key = "invalid.age.range"
  override def args: List[Any] = params.toList
}

case object InvalidEmail extends ValidationErrorType {
  def key = "invalid.email"
}

case object InvalidPhone extends ValidationErrorType {
  def key = "invalid.phone"
}

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

    validateNonEmptyName(name) *> validateUppercaseName(name)
    // validateNonEmptyName(name) productR validateUppercaseName(name) // equivalent
    // validateNonEmptyName(name) combine validateUppercaseName(name) //accumulates all errors AND ALL SUCCESS !

  }

  def validateAge(age: String): AllErrorsOr[Int] = {
    val numericAgeValidation = Try(age.toInt)
      .toEither
      .leftMap(_ => ValidationError("person.age", AgeMustBeNumeric))
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

    /*
    contacts.view
          .zipWithIndex
          .map {
            case (c, i) => (validateEmail(c.email, i), validatePhone(c.phone, i)).mapN(Contact)
          }.toList.sequence
    */


    contacts.view
      .zipWithIndex
      .toList
      .traverse { case (c, i) => (validateEmail(c.email, i), validatePhone(c.phone, i)).mapN(Contact) }

  }

  def validate(p: PersonForm): AllErrorsOr[Person] = (validateName(p.name), validateContacts(p.contacts), validateAge(p.age)).mapN(Person)

  val okName = "Richard"
  val badName = "rc"
  val emptyName = ""
  val okEmail = "richard.capraro@domain.fr"
  val badEmail = "richard.capraro#domain,fr"
  val okPhone = "+33 06 77 77 77 77"
  val badPhone = "???"

  validate(PersonForm(okName, List(Contact(okEmail, okPhone)), "47")) match {
    case Invalid(f) => println(f.toList.groupBy(_.path))
    case Valid(s) => println(s)
  }

  val invalidResult = validate(PersonForm(emptyName, List(Contact(okEmail, badPhone), Contact(badEmail, okPhone)), "???")).fold(
    f => f.toList.groupBy(_.path),
    identity
  )

  println(invalidResult)
}
