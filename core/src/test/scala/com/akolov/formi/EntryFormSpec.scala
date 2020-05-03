package com.akolov.formi

import com.akolov.formi.data.CvTestData
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class EntryFormSpec extends AnyFlatSpecLike with Matchers with CvTestData {
  "Entry form renderer" should "render single group" in {
    val g: SingleGroupFormEntry =
      EntryForm.renderSingleGroup(testTemplate.body, testTemplate.body.singleEmpty).right.get

    g shouldEqual SingleGroupFormEntry(
      "Simple CV",
      List(
        GroupEntry(
          "Head",
          Multiplicity.Once,
          Vector(
            SingleGroupFormEntry(
              "Head",
              List(
                FieldEntry("name", Text(Some(50), None), FieldValue(None)),
                FieldEntry("title", Text(Some(50), None), FieldValue(None)))))
        ),
        GroupEntry(
          "Info",
          Multiplicity.Once,
          Vector(
            SingleGroupFormEntry(
              "Info",
              List(
                FieldEntry("phone", Text(Some(12), None), FieldValue(None)),
                FieldEntry("email", Text(Some(25), None), FieldValue(None)))))
        ),
        GroupEntry(
          "Links",
          Multiplicity.Once,
          Vector(
            SingleGroupFormEntry(
              "Links",
              List(GroupEntry(
                "Link",
                Multiplicity.Once,
                Vector(
                  SingleGroupFormEntry(
                    "Link",
                    List(
                      FieldEntry("linkName", Text(Some(12), None), FieldValue(None)),
                      FieldEntry("linkValue", Text(Some(25), None), FieldValue(None)))))
              ))
            ))
        )
      )
    )
  }
}
