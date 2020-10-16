package com.akolov.formi.data

import com.akolov.formi._

import scala.io.Source

trait CvTestData {
  val nameFieldElement: Field = Field(label = "name", input = Text(maxLength = Some(50), pattern = None))
  val titleFieldElement: Field = Field(label = "title", input = Text(Some(50)))

  val headGroupElement: Group = Group(
    "Head",
    fields = List(
      nameFieldElement,
      titleFieldElement
    ))

  val infoGroupElement: Group = Group(
    "Info",
    fields = List(
      Field(label = "phone", input = Text(Some(12))),
      Field(label = "email", input = Text(Some(25)))
    ))

  val linkGroupElement: Group = Group(
    "Link",
    fields = List(
      Field(label = "linkName", input = Text(Some(12))),
      Field(label = "linkValue", input = Text(Some(25)))
    ),
    multiplicity = Multiplicity.AtLeastOnce
  )

  private val linksGroupElement: Group = Group(
    "Links",
    fields = List(
      linkGroupElement
    )
  )

  val testTemplate = Template(
    name = "Simple CV",
    body = Group(
      "Simple CV",
      List(
        headGroupElement,
        infoGroupElement,
        linksGroupElement
      )
    )
  )
}
