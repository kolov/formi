package com.akolov.formi

import cats.implicits._
import com.akolov.formi.compact.CompactTemplate.CompactTemplateElement
import com.akolov.formi.data.CvTestData
import io.circe.Printer
import io.circe.jawn.decode
import io.circe.syntax._
import org.log4s.getLogger
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class FormCodecsSpec extends AnyFlatSpecLike with Matchers with CvTestData with FormCodecs {
  val logger = getLogger

  val labelsProvider = new LabelsProvider {
    override def findLabel(els: Seq[String]): Option[String] = Some(s"""{${els.mkString(".")}}""")
  }

  val printer = Printer.spaces2.copy(dropNullValues = true)

  "template json encoder" should "print a template" in {
    val json = testTemplate.asJson.printWith(printer)
    logger.info(json)
    json.length should be > 1
  }

  "template json decoder" should "read template" in new TestContext {
    val group = decode[CompactTemplateElement](encoded)
    logger.info(group.toString)
    group.map(_.fields.get.length) shouldBe Right(4)
  }

  "entry form json encoder" should "print a entry form" in {
    val json =
      EntryForm
        .renderEntryForm(labelsProvider, testTemplate.body, testTemplate.body.singleEmpty)
        .right
        .get
        .asJson
        .printWith(printer)
    logger.info(json)
    json.length should be > 1
  }
}

trait TestContext {
  val encoded = Source.fromResource("cv-developer.json").getLines().mkString("\n")
}
