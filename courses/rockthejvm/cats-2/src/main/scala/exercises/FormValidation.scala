package exercises

import cats.data.Validated
import cats.syntax.apply.*
import cats.syntax.validated

object FormValidation extends App:

  type FormValidation[T] = Validated[List[String], T]

  case class UserReg(name: String, email: String, password: String)

  /** fields:
    *   - name
    *   - email
    *   - password
    *
    * rules:
    *   - all fields are mandatory
    *   - name must not be blank
    *   - email must have '@'
    *   - password must have >= 10 characters
    */
  def validateForm(form: Map[String, String]): FormValidation[UserReg] =
    (
      Validated.cond(
        form.get("name").exists(_.trim.nonEmpty),
        form("name"),
        List("invalid name")
      ),
      Validated.cond(
        form.get("email").exists(_.contains("@")),
        form("email"),
        List("invalid email")
      ),
      Validated.cond(
        form.get("password").exists(_.trim.length > 10),
        form("password"),
        List("invalid password")
      )
    ).mapN(UserReg.apply)

  val form = Map(
    "name"     -> "user",
    "email"    -> "user@mail.com",
    "password" -> "10characters"
  )

  println(validateForm(form))

  // Inspired by Daniel
  def validateMandatory(fieldname: String, form: Map[String, String]): FormValidation[String] =
    Validated.fromOption(form.get(fieldname), List("missing field: " + fieldname))

  def validateNonBlank(fieldname: String, form: Map[String, String]): FormValidation[String] =
    Validated.fromOption(
      form.get(fieldname).filter(_.trim.nonEmpty),
      List("empty field: " + fieldname)
    )

  def validateContains(
      fieldname: String,
      contains: String,
      form: Map[String, String]
  ): FormValidation[String] =
    Validated.fromOption(
      form.get(fieldname).filter(_.contains(contains)),
      List(s"field $fieldname does not contain $contains")
    )

  def validateMinLength(
      fieldname: String,
      length: Int,
      form: Map[String, String]
  ): FormValidation[String] =
    Validated.fromOption(
      form.get(fieldname).filter(_.trim.length >= length),
      List(s"field $fieldname is shorter than $length characters")
    )

  def validateFormD(form: Map[String, String]): FormValidation[UserReg] =
    (
      validateMandatory("name", form).andThen(_ => validateNonBlank("name", form)),
      validateMandatory("email", form).andThen(_ => validateContains("email", "@", form)),
      validateMandatory("password", form).andThen(_ => validateMinLength("password", 10, form))
    ).mapN(UserReg.apply)

  println(validateFormD(form))
