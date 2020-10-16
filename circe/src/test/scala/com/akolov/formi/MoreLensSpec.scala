package com.akolov.formi

import com.akolov.formi.lenses.DocumentLenses._
import org.scalatest.matchers.should.Matchers
import cats.implicits._
import com.akolov.formi.compact.CompactTemplate
import com.akolov.formi.data.CvTestData
import com.akolov.formi.lenses._
import org.log4s.getLogger
import org.scalatest.flatspec.AnyFlatSpecLike
import io.circe.parser.decode

class MoreLensSpec extends AnyFlatSpecLike with Matchers with CvTestData with FormCodecs {
  val logger = getLogger

  it should "set values in developer cv" in new CvTestData {
    val cv: CompactTemplate.Group = decode[CompactTemplate.Group](developerCv).right.get

    private val template: Group = cv.expand
    val content = template.singleEmpty

    val newValue = for {
      path <- AnyPath.parsePath("/work-experience[0]/experience-entry[0]/company")
      gfLens <- lensFor(template, path)
      fl <- gfLens.asFieldLens
      updated <- fl.set(content, FieldValue("Acme"))
      newValue <- fl.get(updated)
    } yield newValue

    newValue shouldEqual FieldValue("Acme").asRight
  }
}
