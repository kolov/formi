package com.akolov.formi.data

import com.akolov.formi._

trait CvTestData {
  val nameFieldElement: Field = Field(label = "name", input = InputDesc("text", maxLength = Some(50), pattern = None))
  val titleFieldElement: Field = Field(label = "title", input = InputDesc(`type` = "text", maxLength = Some(50)))

  val headGroupElement: Group = Group(
    "head",
    fields = List(
      nameFieldElement,
      titleFieldElement
    ))

  val infoGroupElement: Group = Group(
    "info",
    fields = List(
      Field(label = "phone", input = InputDesc(`type` = "text", maxLength = Some(12))),
      Field(label = "email", input = InputDesc(`type` = "text", maxLength = (Some(25))))
    )
  )

  val linkGroupElement: Group = Group(
    "link",
    fields = List(
      Field(label = "linkName", input = InputDesc(`type` = "text", maxLength = Some(12))),
      Field(label = "linkValue", input = InputDesc(`type` = "text", maxLength = Some(25)))
    ),
    multiplicity = Multiplicity.AtLeastOnce
  )

  private val linksGroupElement: Group = Group(
    "links",
    fields = List(
      linkGroupElement
    )
  )

  val testTemplate = Template(
    name = "Simple CV",
    body = Group(
      "cv",
      List(
        headGroupElement,
        infoGroupElement,
        linksGroupElement
      )
    )
  )
}
