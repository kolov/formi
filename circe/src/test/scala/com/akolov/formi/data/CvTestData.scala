package com.akolov.formi.data

import com.akolov.formi._

import scala.io.Source

trait CvTestData {
  val nameFieldElement: Field = Field(label = "name", desc = Text(maxLength = Some(50), pattern = None))
  val titleFieldElement: Field = Field(label = "title", desc = Text(Some(50)))

  val headGroupElement: Group = Group(
    "Head",
    fields = List(
      nameFieldElement,
      titleFieldElement
    ))

  val infoGroupElement: Group = Group(
    "Info",
    fields = List(
      Field(label = "phone", desc = Text(Some(12))),
      Field(label = "email", desc = Text(Some(25)))
    ))

  val testTemplate = Template(
    name = "Simple CV",
    body = Group(
      "Simple CV",
      List(
        headGroupElement,
        infoGroupElement,
        Group(
          "Links",
          fields = List(
            Group(
              "Link",
              fields = List(
                Field(label = "linkName", desc = Text(Some(12))),
                Field(label = "linkValue", desc = Text(Some(25)))
              ),
              multiplicity = Multiplicity.AtLeastOnce
            )
          )
        )
      )
    )
  )

  val developerCv = Source.fromResource("cv-developer.json").getLines().mkString("\n")
}
