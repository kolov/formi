package com.akolov.formi

import com.akolov.formi.data.CvTestData
import com.akolov.formi.lenses.GroupInstancePath
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class EntryFormSpec extends AnyFlatSpecLike with Matchers with CvTestData {

  val labelsProvider = new LabelsProvider {
    override def findLabel(els: Seq[String]): Option[String] = None
  }

  "Entry form renderer" should "render single group" in {
    val g: Seq[Entry] =
      EntryForm.renderSingleGroup(testTemplate.body, testTemplate.body.singleEmpty, labelsProvider).right.get

    g shouldEqual
      List(
        GroupEntry(
          "head",
          "{cv.head}",
          Multiplicity.Once,
          List(
            List(
              FieldEntry("name", "{cv.head.name}", Text(Some(50), None), FieldValue(None)),
              FieldEntry("title", "{cv.head.title}", Text(Some(50), None), FieldValue(None))))
        ),
        GroupEntry(
          "info",
          "{cv.info}",
          Multiplicity.Once,
          List(
            List(
              FieldEntry("phone", "{cv.info.phone}", Text(Some(12), None), FieldValue(None)),
              FieldEntry("email", "{cv.info.email}", Text(Some(25), None), FieldValue(None))))
        ),
        GroupEntry(
          "links",
          "{cv.links}",
          Multiplicity.Once,
          List(
            List(
              GroupEntry(
                "link",
                "{cv.links.link}",
                Multiplicity.AtLeastOnce,
                List(List(
                  FieldEntry("linkName", "{cv.links.link.linkName}", Text(Some(12), None), FieldValue(None)),
                  FieldEntry("linkValue", "{cv.links.link.linkValue}", Text(Some(25), None), FieldValue(None))
                ))
              ))
          )
        )
      )
  }
}
