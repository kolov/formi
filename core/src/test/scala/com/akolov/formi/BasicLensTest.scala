package com.akolov.formi

import com.akolov.formi.DocumentLens._
import org.scalatest.matchers.should.Matchers
import cats.implicits._
import com.akolov.formi.errors.IndexError
import org.log4s.getLogger
import org.scalatest.flatspec.AnyFlatSpecLike

class BasicLensTest extends AnyFlatSpecLike with Matchers with CvTestData {
  val logger = getLogger

  val cvGroupElement: Group = testTemplate.body

  "fieldLens" should "get empty contained value from empty field value" in {
    val emptyFieldValue = nameFieldElement.emptyField
    val lens = fieldIndexLens(nameFieldElement, 0)
    lens.get(emptyFieldValue) shouldEqual SingleFieldValue.Empty.asRight
  }

  it should "get error when index out of bounds" in {
    val emptyFieldValue = nameFieldElement.emptyField
    val lens = fieldIndexLens(nameFieldElement, 1)
    lens.get(emptyFieldValue).left.get.isInstanceOf[IndexError] shouldEqual true
  }

  it should "obey set-get law" in {
    val emptyFieldValue = nameFieldElement.emptyField
    val lens = fieldIndexLens(nameFieldElement, 0)
    val r = for {
      updated <- lens.set(emptyFieldValue, SingleFieldValue("3"))
      r <- lens.get(updated)
    } yield r
    r shouldEqual SingleFieldValue("3").asRight
  }

  "GroupLens" should "get empty contained value from empty field value" in {
    val emptyMultiGroupValue = cvGroupElement.emptyGroup
    val lens = groupIndexLens(cvGroupElement, 0)

    lens.get(emptyMultiGroupValue) shouldEqual cvGroupElement.emptySingle.asRight
  }

  it should "get value of head[0]" in {
    val cvSIngleGroupValue = cvGroupElement.emptySingle

    val eitherLens = lensFor(cvGroupElement, Path(List(Indexed("Head"))))
    logger.debug(s"eitherLens for head[0]: $eitherLens")
    val gfLens: GFLens = eitherLens.right.get
    logger.debug(s"gfLens for head[0]: $gfLens")
    gfLens.asFieldLens.isLeft shouldBe true

    val groupLens = gfLens.asGroupLens.right.get

    groupLens.get(cvSIngleGroupValue) shouldEqual headGroupElement.emptySingle.asRight
  }

  it should "set value in name[0] and get it back" in {
    val headSingleGroupValue = headGroupElement.emptySingle

    val newValue = for {
      gfLens <- lensFor(headGroupElement, Path(List(Indexed("name"))))
      fl <- gfLens.asFieldLens
      updated <- fl.set(headSingleGroupValue, SingleFieldValue("George Costanza"))
      newValue <- fl.get(updated)
    } yield newValue

    newValue shouldEqual SingleFieldValue("George Costanza").asRight
  }

  it should "get value from head[0]/name[0]  " in {
    val cvSIngleGroupValue = cvGroupElement.emptySingle

    val eitherLens = for {
      gfLens <- lensFor(cvGroupElement, Path(List(Indexed("Head"), Indexed("name"))))
      fl <- gfLens.asFieldLens
    } yield fl
    println(eitherLens)
    val lens: DocumentLens[SingleGroupValue, SingleFieldValue] = eitherLens.right.get

    lens.get(cvSIngleGroupValue) shouldEqual nameFieldElement.emptySingle.asRight
  }

  it should "set value in head[0]/name[0] and get it back" in {
    val cvSingleGroupValue = cvGroupElement.emptySingle

    val newValue = for {
      gfLens <- lensFor(cvGroupElement, Path(List(Indexed("Head"), Indexed("name"))))
      fl <- gfLens.asFieldLens
      updated <- fl.set(cvSingleGroupValue, SingleFieldValue("George Costanza"))
      newValue <- fl.get(updated)
    } yield newValue

    newValue shouldEqual SingleFieldValue("George Costanza").asRight
  }
}
