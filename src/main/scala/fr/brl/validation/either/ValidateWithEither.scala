package fr.brl.validation.either

import fr.brl.validation.utils.Validators

import scala.collection.mutable.ListBuffer

object ValidateWithEither extends App {

  case class Data(name: String, email: String, age: Int)

  def validateName(n: String): Either[List[String], String] = {
    val errors = new ListBuffer[String]()
    if (n.isBlank) errors += "Name should not be blank"
    if (!n.exists(_.isUpper)) errors += "Name should begin with uppercase"
    if (n.length <= 3) errors += "Name is too short"
    if (n.length > 100) errors += "Name is too long"

    if (errors.nonEmpty) Left(errors.toList) else Right(n)
  }

  def validateEmail(e: String): Either[List[String], String] = if (Validators.isValidEmail(e)) Right(e) else Left(List("Invalid email"))

  def validateAge(a: Int): Either[List[String], Int] = {
    val errors = new ListBuffer[String]()
    if (a <= 0) errors += "Age should be greater than 0"
    if (a > 150) errors += "Age should be lower than 150"

    if (errors.nonEmpty) Left(errors.toList) else Right(a)
  }

  def validate(d: Data): Either[List[String], Data] =
    for {
      validName <- validateName(d.name)
      validEmail <- validateEmail(d.email)
      validAge <- validateAge(d.age)
    } yield Data(validName, validEmail, validAge)

  val okName = "Richard"
  val badName = "he"
  val okEmail = "richard.capraro@brl.fr"
  val badEmail = "richard.capraro#brl,fr"
  println(validate(Data(okName, okEmail, 47)))
  println(validate(Data("", okEmail, 47)))
  println(validate(Data(badName, badEmail, -1)))   // Problem : if the name is invalid , it will not validate the email !


}
