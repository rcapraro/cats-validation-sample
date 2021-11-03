package fr.brl.validation.option

import fr.brl.validation.utils.Validators

object ValidationWithOption extends App {

  case class Data(name: String, email: String, age: Int)

  def validateName(n: String): Option[String] = Option.when(!n.isBlank && n.exists(_.isUpper))(n)

  def validateEmail(e: String): Option[String] = Option.when(Validators.isValidEmail(e))(e)

  def validateAge(a: Int): Option[Int] = Option.when(a > 0 && a < 150)(a)

  def validate(d: Data) = for {
    validName <- validateName(d.name)
    validEmail <- validateEmail(d.email)
    validAge <- validateAge(d.age)
  } yield Data(validName, validEmail, validAge)


  val okName = "Richard"
  val badName = "he"
  val okEmail = "richard.capraro@brl.fr"
  val badEmail = "richard.capraro#brl,fr"
  println(validate(Data(okName, okEmail, 47)))
  println(validate(Data("", okEmail, 47))) //None
  println(validate(Data(badName, badEmail, -1))) //None
  // Problem : You can't say what is wrong !
}
