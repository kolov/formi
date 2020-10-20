package com.akolov.formi

import com.akolov.formi.data.CvTestData
import com.akolov.formi.lenses.GroupInstancePath
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class EntryFormSpec extends AnyFlatSpecLike with Matchers with CvTestData {

  val labelsProvider = new LabelsProvider {
    override def findLabel(els: Seq[String]): Option[String] = Some(s"""{${els.mkString(".")}}""")
  }

  "Entry form renderer" should "render single group" in {
    val g: Seq[Entry] =
      EntryForm
        .renderEntryForm(labelsProvider, testTemplate.body, testTemplate.body.singleEmpty)
        .right
        .get

    g shouldEqual
      List(
        GroupEntry(
          "head",
          "{cv.head}",
          Multiplicity.Once,
          List(
            List(
              FieldEntry("name", "{cv.head.name}", InputDesc(`type` = "text", maxLength = Some(50)), FieldValue(None)),
              FieldEntry("title", "{cv.head.title}", InputDesc(`type` = "text", maxLength = Some(50)), FieldValue(None))
            ))
        ),
        GroupEntry(
          "info",
          "{cv.info}",
          Multiplicity.Once,
          List(
            List(
              FieldEntry(
                "phone",
                "{cv.info.phone}",
                InputDesc(`type` = "text", maxLength = Some(12)),
                FieldValue(None)),
              FieldEntry("email", "{cv.info.email}", InputDesc(`type` = "text", maxLength = Some(25)), FieldValue(None))
            ))
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
                  FieldEntry(
                    "linkName",
                    "{cv.links.link.linkName}",
                    InputDesc(`type` = "text", maxLength = Some(12)),
                    FieldValue(None)),
                  FieldEntry(
                    "linkValue",
                    "{cv.links.link.linkValue}",
                    InputDesc(`type` = "text", maxLength = Some(25)),
                    FieldValue(None))
                ))
              ))
          )
        )
      )
  }
}
