package com.akolov.formi.data

import com.akolov.formi._

import scala.io.Source

trait CvTestData {

  val nameFieldElement: Field =
    Field(label = "name", input = InputDesc(`type` = "text", maxLength = Some(50), pattern = None))
  val titleFieldElement: Field = Field(label = "title", input = InputDesc(`type` = "text", maxLength = Some(50)))

  val headGroupElement: Group = Group(
    "Head",
    fields = List(
      nameFieldElement,
      titleFieldElement
    ))

  val infoGroupElement: Group = Group(
    "Info",
    fields = List(
      Field(label = "phone", input = InputDesc(`type` = "text", maxLength = Some(50))),
      Field(label = "email", input = InputDesc(`type` = "text", maxLength = Some(50)))
    )
  )

  val testTemplate = Template(
    name = "Simple CV",
    body = Group(
      "cv",
      List(
        headGroupElement,
        infoGroupElement,
        Group(
          "links",
          fields = List(
            Group(
              "link",
              fields = List(
                Field(label = "linkName", input = InputDesc(`type` = "text", maxLength = Some(50))),
                Field(label = "linkValue", input = InputDesc(`type` = "text", maxLength = Some(50)))
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
