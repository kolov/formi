package com.akolov.formi

import cats.implicits._
import com.akolov.formi.data.CvTestData
import com.akolov.formi.lenses.DocumentLens._
import org.log4s.getLogger
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class LensMultiplicitySpec extends AnyFlatSpecLike with Matchers with CvTestData {
  val logger = getLogger

  "fieldLens" should "get empty value 0 from empty field 0 to 1" in {
    val field: Field =
      Field(label = "name", desc = Text(), multiplicity = Multiplicity.Optional)
    val emptyFieldValue = field.emptyField
    val lens = fieldIndexLens(nameFieldElement, 0)
    lens.get(emptyFieldValue) shouldEqual SingleFieldValue.Empty.asRight
  }

  "fieldLens" should "get empty value 0 from empty field 1 to 1" in {
    val field: Field =
      Field(label = "name", desc = Text(), multiplicity = Multiplicity.Once)
    val emptyFieldValue = field.emptyField
    val lens = fieldIndexLens(nameFieldElement, 0)
    lens.get(emptyFieldValue) shouldEqual SingleFieldValue.Empty.asRight
  }

  "fieldLens" should "get empty 1 value from field 2 to 2" in {
    val field: Field =
      Field(label = "name", desc = Text(), multiplicity = Multiplicity(2, Some(2)))
    val emptyFieldValue = field.emptyField
    fieldIndexLens(field, 0).get(emptyFieldValue) shouldEqual SingleFieldValue.Empty.asRight
    fieldIndexLens(field, 1).get(emptyFieldValue) shouldEqual SingleFieldValue.Empty.asRight
  }

  "fieldLens" should "insert at index 2 in field 2 to many" in {
    val field: Field =
      Field(label = "element2toMany", desc = Text(), multiplicity = Multiplicity(2))
    val emptyFieldValue = field.emptyField
    fieldIndexLens(field, 0).get(emptyFieldValue) shouldEqual SingleFieldValue.Empty.asRight
    fieldIndexLens(field, 0).set(emptyFieldValue, SingleFieldValue("0")).isRight shouldEqual true
    fieldIndexLens(field, 1).get(emptyFieldValue) shouldEqual SingleFieldValue.Empty.asRight
    fieldIndexLens(field, 1).set(emptyFieldValue, SingleFieldValue("1")).isRight shouldEqual true

    fieldIndexLens(field, 2).get(emptyFieldValue).isLeft shouldEqual true
    fieldIndexLens(field, 2).set(emptyFieldValue, SingleFieldValue("2")).isRight shouldEqual true

    fieldIndexLens(field, 3).get(emptyFieldValue).isLeft shouldEqual true
    fieldIndexLens(field, 3).set(emptyFieldValue, SingleFieldValue("2")).isLeft shouldEqual true
  }
}
