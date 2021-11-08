package fr.brl.validation.validated

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import fr.brl.validation.utils.Validators

import scala.collection.mutable.ListBuffer

object ValidateWithValidated extends App {

  case class Data(name: String, email: String, age: Int)

  def validateName(n: String): Validated[List[String], String] = {
    val errors = new ListBuffer[String]()
    if (n.isEmpty) errors += "Name should not be blank"
    if (!n.exists(_.isUpper)) errors += "Name should begin with uppercase"
    if (n.length <= 3) errors += "Name is too short"
    if (n.length > 100) errors += "Name is too long"

    if (errors.nonEmpty) Invalid(errors.toList) else Valid(n)
  }

  def validateEmail(e: String): Validated[List[String], String] =
    if (Validators.isValidEmail(e)) Valid(e) else Invalid(List("Invalid email"))

  def validateAge(a: Int): Validated[List[String], Int] = {
    val errors = new ListBuffer[String]()
    if (a <= 0) errors += "Age should be greater than 0"
    if (a > 150) errors += "Age should be lower than 150"

    if (errors.nonEmpty) Invalid(errors.toList) else Valid(a)
  }

  def validate(d: Data): Validated[List[String], Data] = {
    val validName = validateName(d.name)
    val validEmail =  validateEmail(d.email)
    val validAge = validateAge(d.age)
    (validName, validEmail,validAge).mapN(Data)
  }

  val okName = "Richard"
  val badName = "he"
  val okEmail = "richard.capraro@brl.fr"
  val badEmail = "richard.capraro#brl,fr"
  println(validate(Data(okName, okEmail, 47)))
  println(validate(Data("", okEmail, 47)))
  println(validate(Data(badName, badEmail, -1)))

  // Problem: we can have Invalid with en emptyList:

}
