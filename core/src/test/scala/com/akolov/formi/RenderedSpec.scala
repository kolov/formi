package com.akolov.formi

import cats.implicits._
import com.akolov.formi.Rendered._
import org.log4s.getLogger
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class RenderedSpec extends AnyFlatSpecLike with Matchers {
  val logger = getLogger

  "field renderer" should "render field with valu" in {
    val nameFieldElement: Field =
      Field(label = "name", InputDesc("text"))
    val entry = Rendered.render(nameFieldElement, FieldValue("George"))
    entry shouldEqual FieldView("name", Some("George")).asRight
  }

  "group renderer" should "render error on wrong value " in {
    val firstName: Field = Field("firstName", InputDesc("text"))
    val secondName: Field = Field("secondName", InputDesc("text"))
    val group = Group("name", List(firstName, secondName))
    val entry = Rendered.render(group, FieldValue("George"))
    entry.isLeft shouldEqual true
  }

  "group renderer" should "render error on missing group value value " in {
    val firstName: Field = Field("firstName", InputDesc("text"))
    val secondName: Field = Field("secondName", InputDesc("text"))
    val group = Group("name", List(firstName, secondName))
    val entry = Rendered.render(group, GroupValue(Vector(SingleGroupValue(Map("xxx" -> FieldValue("George"))))))
    entry.isLeft shouldEqual true
  }

  "group renderer" should "render error on good value " in {
    val firstName: Field = Field("firstName", InputDesc("text"))
    val secondName: Field = Field("secondName", InputDesc("text"))
    val group = Group("fullName", List(firstName, secondName))
    val entry = Rendered.render(
      group,
      GroupValue(
        Vector(SingleGroupValue(Map("firstName" -> FieldValue("George"), "secondName" -> FieldValue("Costanza"))))))

    entry shouldEqual GroupView(
      "fullName",
      Vector(
        SingleGroupView(
          "fullName",
          List(FieldView("firstName", Some("George")), FieldView("secondName", Some("Costanza")))))
    ).asRight
  }
}
