package com.akolov.formi

import cats.implicits._
import com.akolov.formi.errors.IndexError
import com.akolov.formi.lenses.DocumentLens._
import com.akolov.formi.lenses.{Indexed, Path}
import org.log4s.getLogger
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class DocumentSpec extends AnyFlatSpecLike with Matchers {
  val logger = getLogger

  val nameFieldElement: Field =
    Field(label = "v", desc = Text(maxLength = Some(50), pattern = None), multiplicity = Multiplicity.AtLeastOnce)

  "fieldLens" should "get empty contained value from empty field value" in {
    val emptyFieldValue = nameFieldElement.emptyField
    val lens = fieldIndexLens(nameFieldElement, 0)
    lens.get(emptyFieldValue) shouldEqual SingleFieldValue.Empty.asRight
  }
}
