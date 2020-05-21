package com.akolov.formi

import com.akolov.formi.data.CvTestData
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class EntryFormSpec extends AnyFlatSpecLike with Matchers with CvTestData {
  "Entry form renderer" should "render single group" in {
    val g: Seq[Entry] =
      EntryForm.renderSingleGroup(testTemplate.body, testTemplate.body.singleEmpty).right.get

    g shouldEqual
      List(
        GroupEntry(
          "Head",
          Multiplicity.Once,
          List(
            List(
              FieldEntry("name", Text(Some(50), None), FieldValue(None)),
              FieldEntry("title", Text(Some(50), None), FieldValue(None))))
        ),
        GroupEntry(
          "Info",
          Multiplicity.Once,
          List(
            List(
              FieldEntry("phone", Text(Some(12), None), FieldValue(None)),
              FieldEntry("email", Text(Some(25), None), FieldValue(None))))
        ),
        GroupEntry(
          "Links",
          Multiplicity.Once,
          List(
            List(
              GroupEntry(
                "Link",
                Multiplicity.AtLeastOnce,
                List(
                  List(
                    FieldEntry("linkName", Text(Some(12), None), FieldValue(None)),
                    FieldEntry("linkValue", Text(Some(25), None), FieldValue(None))))
              ))
          )
        )
      )
  }
}
