package com.akolov.formi

import com.akolov.formi.lenses.DocumentLenses._
import org.scalatest.matchers.should.Matchers
import cats.implicits._
import com.akolov.formi.data.CvTestData
import com.akolov.formi.lenses._
import org.log4s.getLogger
import org.scalatest.flatspec.AnyFlatSpecLike

class BasicLensSpec extends AnyFlatSpecLike with Matchers with CvTestData {
  val logger = getLogger

  val cvGroupElement: Group = testTemplate.body

  it should "get value from head[0]/name  " in {
    val cvSIngleGroupValue = cvGroupElement.singleEmpty

    val eitherLens = for {
      gfLens <- lensFor(cvGroupElement, Path(List(Indexed("Head")), Some(Named("name"))))
      fl <- gfLens.asFieldLens
    } yield fl
    println(eitherLens)
    val lens: DocumentLens[SingleGroupValue, FieldValue] = eitherLens.right.get

    lens.get(cvSIngleGroupValue) shouldEqual nameFieldElement.empty.asRight
  }

  it should "set value in head[0]/name[0] and get it back" in {
    val cvSingleGroupValue = cvGroupElement.singleEmpty

    val newValue = for {
      gfLens <- lensFor(cvGroupElement, Path(List(Indexed("Head")), Some(Named("name"))))
      fl <- gfLens.asFieldLens
      updated <- fl.set(cvSingleGroupValue, FieldValue("George Costanza"))
      newValue <- fl.get(updated)
    } yield newValue

    newValue shouldEqual FieldValue("George Costanza").asRight
  }

  it should "set values in developer cv" in {
    val cvSingleGroupValue = cvGroupElement.singleEmpty

    val newValue = for {
      gfLens <- lensFor(cvGroupElement, Path(List(Indexed("Head")), Some(Named("name"))))
      fl <- gfLens.asFieldLens
      updated <- fl.set(cvSingleGroupValue, FieldValue("George Costanza"))
      newValue <- fl.get(updated)
    } yield newValue

    newValue shouldEqual FieldValue("George Costanza").asRight
  }
}
