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
        .renderSingleGroup(GroupInstancePath.empty, testTemplate.body, testTemplate.body.singleEmpty)
        .run(labelsProvider)
        .right
        .get

    g shouldEqual
      List(
        GroupEntry(
          "head",
          "{head}",
          Multiplicity.Once,
          List(
            List(
              FieldEntry("name", "{name}", Text(Some(50), None), FieldValue(None)),
              FieldEntry("title", "{title}", Text(Some(50), None), FieldValue(None))))
        ),
        GroupEntry(
          "info",
          "{info}",
          Multiplicity.Once,
          List(
            List(
              FieldEntry("phone", "{phone}", Text(Some(12), None), FieldValue(None)),
              FieldEntry("name", "{name}", Text(Some(25), None), FieldValue(None))))
        ),
        GroupEntry(
          "links",
          "{links}",
          Multiplicity.Once,
          List(
            List(
              GroupEntry(
                "link",
                "{link}",
                Multiplicity.AtLeastOnce,
                List(
                  List(
                    FieldEntry("linkName", "{linkName}", Text(Some(12), None), FieldValue(None)),
                    FieldEntry("linkValue", "{linkValue}", Text(Some(25), None), FieldValue(None))))
              ))
          )
        )
      )
  }
}
