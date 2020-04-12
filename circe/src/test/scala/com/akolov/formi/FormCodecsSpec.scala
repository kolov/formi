package com.akolov.formi

import cats.implicits._
import com.akolov.formi.data.CvTestData
import io.circe.Printer
import io.circe.syntax._
import org.log4s.getLogger
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class FormCodecsSpec extends AnyFlatSpecLike with Matchers with CvTestData with FormCodecs {
  val logger = getLogger

  val printer = Printer.spaces2.copy(dropNullValues = true)

  "template json encoder" should "print a template" in {
    val json = testTemplate.asJson.printWith(printer)
    logger.info(json)
    json.length should be > 1
  }
}
