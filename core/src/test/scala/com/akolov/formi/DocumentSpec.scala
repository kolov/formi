package com.akolov.formi

import cats.implicits._
import org.log4s.getLogger
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class DocumentSpec extends AnyFlatSpecLike with Matchers {
  val logger = getLogger

  "field renderer" should "render field with valu" in {
    val nameFieldElement: Field =
      Field(label = "name", Text())
    val entry = DocumentEntry.render(nameFieldElement, FieldValue("George"))
    entry shouldEqual FieldDocumentEntry("name", Some("George")).asRight
  }
  "group renderer" should "render error on wrong value " in {
    val firstName: Field = Field("firstName", Text())
    val secondName: Field = Field("secondName", Text())
    val group = Group("name", List(firstName, secondName))
    val entry = DocumentEntry.render(group, FieldValue("George"))
    entry.isLeft shouldEqual true
  }

  "group renderer" should "render error on missing group value value " in {
    val firstName: Field = Field("firstName", Text())
    val secondName: Field = Field("secondName", Text())
    val group = Group("name", List(firstName, secondName))
    val entry = DocumentEntry.render(group, GroupValue(Vector(SingleGroupValue(Map("xxx" -> FieldValue("George"))))))
    entry.isLeft shouldEqual true
  }

  "group renderer" should "render error on good value " in {
    val firstName: Field = Field("firstName", Text())
    val secondName: Field = Field("secondName", Text())
    val group = Group("name", List(firstName, secondName))
    val entry = DocumentEntry.render(
      group,
      GroupValue(
        Vector(SingleGroupValue(Map("firstName" -> FieldValue("George"), "secondName" -> FieldValue("Costanza"))))))
    entry shouldEqual GroupDocumentEntry(
      "name",
      Vector(
        SingleGroupDocumentEntry(
          "name",
          List(FieldDocumentEntry("firstName", Some("George")), FieldDocumentEntry("secondName", Some("Costanza")))))
    ).asRight
  }
}
