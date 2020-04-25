package com.akolov.formi

import cats.implicits._
import com.akolov.formi.data.CvTestData
import com.akolov.formi.errors.DocumentError
import com.akolov.formi.lenses.DocumentLenses
import org.log4s.getLogger
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import com.akolov.formi.lenses.DocumentLenses._

class LensMultiplicitySpec extends AnyFlatSpecLike with Matchers with CvTestData {
  val logger = getLogger

  "lens" should "get empty value 0 from empty group 0 to 1" in {
    val innerGroup = Group("inner", List.empty, Multiplicity.Optional)
    val outerGroup = Group("outer", List(innerGroup), Multiplicity.Optional)
    val outerGroupGroupValue: SingleGroupValue = outerGroup.singleEmpty
    val eitherLens: Either[DocumentError, DocumentLenses.DocumentLens[SingleGroupValue, SingleGroupValue]] =
      groupLensFor(outerGroup, Path(List(Indexed("inner", 0)), None))
    eitherLens.isRight shouldEqual true
    eitherLens.right.get.get(outerGroupGroupValue) shouldEqual innerGroup.singleEmpty.asRight
  }

  "lens" should "get empty value 0 from empty group 1 to 1" in {
    val innerGroup = Group("inner", List.empty, Multiplicity.Once)
    val outerGroup = Group("outer", List(innerGroup), Multiplicity.Optional)
    val outerGroupGroupValue: SingleGroupValue = outerGroup.singleEmpty
    val eitherLens: Either[DocumentError, DocumentLenses.DocumentLens[SingleGroupValue, SingleGroupValue]] =
      groupLensFor(outerGroup, Path(List(Indexed("inner", 0)), None))
    eitherLens.isRight shouldEqual true
    eitherLens.right.get.get(outerGroupGroupValue) shouldEqual innerGroup.singleEmpty.asRight
  }
  "lens" should "get empty value 1 from empty group 2 to 2" in {
    val innerGroup = Group("inner", List.empty, Multiplicity(2, 2))
    val outerGroup = Group("outer", List(innerGroup), Multiplicity.Once)
    val outerGroupGroupValue: SingleGroupValue = outerGroup.singleEmpty
    val eitherLens: Either[DocumentError, DocumentLenses.DocumentLens[SingleGroupValue, SingleGroupValue]] =
      groupLensFor(outerGroup, Path(List(Indexed("inner", 1)), None))
    eitherLens.isRight shouldEqual true
    eitherLens.right.get.get(outerGroupGroupValue) shouldEqual innerGroup.singleEmpty.asRight
  }

  "lens" should "insert at index 2 in field 2 to many" in { // FIXME
    val innerGroup = Group("inner", List.empty, Multiplicity(2))
    val outerGroup = Group("outer", List(innerGroup), Multiplicity.Once)
    val outerGroupGroupValue: SingleGroupValue = outerGroup.singleEmpty
    val eitherLens: Either[DocumentError, DocumentLenses.DocumentLens[SingleGroupValue, SingleGroupValue]] =
      groupLensFor(outerGroup, Path(List(Indexed("inner", 1)), None))
    eitherLens.isRight shouldEqual true
    eitherLens.right.get.get(outerGroupGroupValue) shouldEqual innerGroup.singleEmpty.asRight
  }
//  "fieldLens" should "insert at index 2 in field 2 to many" in {
//    val field: Field =
//      Field(label = "element2toMany", desc = Text(), multiplicity = Multiplicity(2))
//    val emptyFieldValue = field.emptyField
//    fieldIndexLens(field, 0).get(emptyFieldValue) shouldEqual FieldValue.Empty.asRight
//    fieldIndexLens(field, 0).set(emptyFieldValue, FieldValue("0")).isRight shouldEqual true
//    fieldIndexLens(field, 1).get(emptyFieldValue) shouldEqual FieldValue.Empty.asRight
//    fieldIndexLens(field, 1).set(emptyFieldValue, FieldValue("1")).isRight shouldEqual true
//
//    fieldIndexLens(field, 2).get(emptyFieldValue).isLeft shouldEqual true
//    fieldIndexLens(field, 2).set(emptyFieldValue, FieldValue("2")).isRight shouldEqual true
//
//    fieldIndexLens(field, 3).get(emptyFieldValue).isLeft shouldEqual true
//    fieldIndexLens(field, 3).set(emptyFieldValue, FieldValue("2")).isLeft shouldEqual true
//  }
}
