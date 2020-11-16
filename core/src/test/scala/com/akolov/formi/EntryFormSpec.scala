package com.akolov.formi

import com.akolov.formi.data.CvTestData
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
          "{cv.head.instance}",
          Multiplicity.Once,
          List(
            List(
              FieldEntry(
                "name",
                "{cv.head.name}",
                InputDesc(`type` = "text", maxLength = Some(50)),
                FieldValue(None),
                Some("{cv.head.name.hint}")),
              FieldEntry(
                "title",
                "{cv.head.title}",
                InputDesc(`type` = "text", maxLength = Some(50)),
                FieldValue(None),
                Some("{cv.head.title.hint}"))
            )),
          Some("{cv.head.hint}")
        ),
        GroupEntry(
          "info",
          "{cv.info}",
          "{cv.info.instance}",
          Multiplicity.Once,
          List(
            List(
              FieldEntry(
                "phone",
                "{cv.info.phone}",
                InputDesc(`type` = "text", maxLength = Some(12)),
                FieldValue(None),
                Some("{cv.info.phone.hint}")),
              FieldEntry(
                "email",
                "{cv.info.email}",
                InputDesc(`type` = "text", maxLength = Some(25)),
                FieldValue(None),
                Some("{cv.info.email.hint}"))
            )),
          Some("{cv.info.hint}")
        ),
        GroupEntry(
          "links",
          "{cv.links}",
          "{cv.links.instance}",
          Multiplicity.Once,
          List(
            List(
              GroupEntry(
                "link",
                "{cv.links.link}",
                "{cv.links.link.instance}",
                Multiplicity.AtLeastOnce,
                List(List(
                  FieldEntry(
                    "linkName",
                    "{cv.links.link.linkName}",
                    InputDesc(`type` = "text", maxLength = Some(12)),
                    FieldValue(None),
                    Some("{cv.links.link.linkName.hint}")),
                  FieldEntry(
                    "linkValue",
                    "{cv.links.link.linkValue}",
                    InputDesc(`type` = "text", maxLength = Some(25)),
                    FieldValue(None),
                    Some("{cv.links.link.linkValue.hint}"))
                )),
                Some("{cv.links.link.hint}")
              ))
          ),
          Some("{cv.links.hint}")
        )
      )
  }
}
