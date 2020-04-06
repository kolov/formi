package com.akolov.forms

trait CvTestData {
  val nameFieldElement: FieldElement = FieldElement(label = "name", desc = Text(maxLength = Some(50), pattern = None))
  val titleFieldElement: FieldElement = FieldElement(label = "title", desc = Text(Some(50)))

  val headGroupElement: GroupElement = GroupElement(
    "Head",
    fields = List(
      nameFieldElement,
      titleFieldElement
    ))

  val infoGroupElement: GroupElement = GroupElement(
    "Info",
    fields = List(
      FieldElement(label = "phone", desc = Text(Some(12))),
      FieldElement(label = "email", desc = Text(Some(25)))
    ))

  val testTemplate = Template(
    name = "Simple CV",
    body = GroupElement(
      "Simple CV",
      List(
        headGroupElement,
        infoGroupElement,
        GroupElement(
          "Links",
          fields = List(
            GroupElement(
              "Link",
              fields = List(
                FieldElement(label = "linkName", desc = Text(Some(12))),
                FieldElement(label = "linkValue", desc = Text(Some(25)))
              ),
              multiplicity = Multiplicity.AtLeastOnce
            )
          )
        )
      )
    )
  )
}
