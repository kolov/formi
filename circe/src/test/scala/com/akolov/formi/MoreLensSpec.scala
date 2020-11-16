package com.akolov.formi

import com.akolov.formi.lenses.DocumentLenses._
import org.scalatest.matchers.should.Matchers
import cats.implicits._
import com.akolov.formi.compact.CompactTemplate
import com.akolov.formi.compact.CompactTemplate.CompactTemplateElement
import com.akolov.formi.data.CvTestData
import com.akolov.formi.lenses._
import org.log4s.getLogger
import org.scalatest.flatspec.AnyFlatSpecLike
import io.circe.parser.decode

class MoreLensSpec extends AnyFlatSpecLike with Matchers with CvTestData with FormCodecs {
  val logger = getLogger

  val decoded = decode[CompactTemplateElement](developerCv)
  val cv: CompactTemplateElement = decoded.right.get

  it should "set nested values in developer cv" in new CvTestData {
    private val template: Group = CompactTemplate.expandGroup(cv).get
    val content = template.singleEmpty

    val newValue = for {
      path <- Path.parsePath("/work-experience[0]/position-head[0]/company")
      gfLens <- lensFor(template, path)
      fl <- gfLens.asFieldLens
      updated <- fl.set(content, FieldValue("Acme"))
      newValue <- fl.get(updated)
    } yield newValue

    newValue shouldEqual FieldValue("Acme").asRight
  }

  it should "set nested multifield values in developer cv" in new CvTestData {
    private val template: Group = CompactTemplate.expandGroup(cv).get
    val content = template.singleEmpty

    val newValue = for {
      path <- Path.parsePath("/summary[0]/core-skills[0]/core-skills-line[0]/text")
      gfLens <- lensFor(template, path)
      fl <- gfLens.asFieldLens
      updated <- fl.set(content, FieldValue("Scala, Java"))
      newValue <- fl.get(updated)
    } yield newValue

    newValue shouldEqual FieldValue("Scala, Java").asRight
  }
}
