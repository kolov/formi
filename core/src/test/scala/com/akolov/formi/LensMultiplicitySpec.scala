package com.akolov.formi

import cats.implicits._
import com.akolov.formi.data.CvTestData
import com.akolov.formi.errors._
import com.akolov.formi.lenses._
import org.log4s.getLogger
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import com.akolov.formi.lenses.DocumentLenses._
import com.akolov.formi.lenses.syntax._

class LensMultiplicitySpec extends AnyFlatSpecLike with Matchers with CvTestData {
  val logger = getLogger

  "lens" should "get empty value 0 from empty group 0 to 1" in {
    val innerGroup = Group("inner", List.empty, Multiplicity.Optional)
    val outerGroup = Group("outer", List(innerGroup), Multiplicity.Optional)
    val outerGroupGroupValue: SingleGroupValue = outerGroup.singleEmpty
    val eitherLens: Either[DocumentError, DocumentLens[SingleGroupValue, SingleGroupValue]] =
      singleGroupLensFor(outerGroup, Path(List(Indexed("inner", 0)), None))
    eitherLens.map(_ => ()) shouldEqual Right(())
    eitherLens.right.get.get(outerGroupGroupValue) shouldEqual innerGroup.singleEmpty.asRight
  }

  "lens" should "get empty value 0 from empty group 1 to 1" in {
    val innerGroup = Group("inner", List.empty, Multiplicity.Once)
    val outerGroup = Group("outer", List(innerGroup), Multiplicity.Optional)
    val outerGroupGroupValue: SingleGroupValue = outerGroup.singleEmpty
    val eitherLens: Either[DocumentError, DocumentLens[SingleGroupValue, SingleGroupValue]] =
      singleGroupLensFor(outerGroup, Path(List(Indexed("inner", 0)), None))
    eitherLens.isRight shouldEqual true
    eitherLens.right.get.get(outerGroupGroupValue) shouldEqual innerGroup.singleEmpty.asRight
  }
  "lens" should "get empty value 1 from empty group 2 to 2" in {
    val innerGroup = Group("inner", List.empty, Multiplicity(2))
    val outerGroup = Group("outer", List(innerGroup), Multiplicity.Once)
    val outerGroupGroupValue: SingleGroupValue = outerGroup.singleEmpty
    val v = for {
      path <- Path.parsePath("inner[1]")
      lens <- singleGroupLensFor(outerGroup, path)
      v <- lens.get(outerGroupGroupValue)
    } yield v
    v shouldEqual innerGroup.singleEmpty.asRight
  }

  "lens" should "fail getting from index 2 in field 2 to many" in {
    val innerGroup = Group("inner", List.empty, Multiplicity(2))
    val outerGroup = Group("outer", List(innerGroup), Multiplicity.Once)
    val outerGroupGroupValue: SingleGroupValue = outerGroup.singleEmpty
    val v = for {
      path <- Path.parsePath("inner[2]")
      lens <- singleGroupLensFor(outerGroup, path)
      v <- lens.get(outerGroupGroupValue)
    } yield v
    v.isLeft shouldEqual true
  }

  "lens" should "insert an empty group at index 2 when 'at least 2''" in {
    val innerGroup = Group("inner", List(Field("text", Text())), Multiplicity.atLeast(2))
    val outerGroup = Group("outer", List(innerGroup), Multiplicity.Once)
    val sgv: SingleGroupValue = outerGroup.singleEmpty
    (for {
      groupLens2 <- singleGroupLensFor(outerGroup, "inner[2]")
      v1 <- sgv.updateField("inner[0]/text", outerGroup, FieldValue("0"))
      v2 <- v1.updateField("inner[1]/text", outerGroup, FieldValue("1"))
      v3 <- groupLens2.set(v2, innerGroup.singleEmpty)
      _ <- v3.insertAt(outerGroup, "inner", 2)
      group2Value <- groupLens2.get(v3)
      field0Value <- v3.getField("inner[0]/text", outerGroup)
      field1Value <- v3.getField("inner[1]/text", outerGroup)
      field2Value <- v3.getField("inner[2]/text", outerGroup)
    } yield (group2Value, field0Value, field1Value, field2Value)) match {
      case Right((group2Value, field0Value, field1Value, field2Value)) =>
        group2Value shouldEqual innerGroup.singleEmpty
        field0Value shouldEqual FieldValue("0")
        field1Value shouldEqual FieldValue("1")
        field2Value shouldEqual FieldValue.Empty
      case Left(e) => fail(e.toString)
    }
  }

  "lens" should "insert new empty link in the Test CV template" in {
    val outerGroup = testTemplate.body
    val root: SingleGroupValue = outerGroup.singleEmpty

    // make sure there is no Links[0]/Link[1]
    root.getGroup("links[0]/link[1]", outerGroup).isLeft shouldBe true

    val link1Updated = for {
      v1 <- root.updateField("links[0]/link[0]/linkName", outerGroup, FieldValue("XXX"))
      rootUpdated <- v1.insertAt(outerGroup, "links[0]/link", 1)
      link1Updated <- rootUpdated.getGroup("links[0]/link[1]", outerGroup)
    } yield link1Updated

    link1Updated shouldEqual linkGroupElement.singleEmpty.asRight

// Links[0] still has 1 link
    val links0Updated = for {
      rootUpdated <- root.insertAt(outerGroup, "links[0]/link", 0)
      links0Updated <- rootUpdated.getGroup("links[0]", outerGroup)
    } yield links0Updated

    links0Updated.right.get.getElement("link").right.get shouldEqual GroupValue(
      Seq(linkGroupElement.singleEmpty, linkGroupElement.singleEmpty))
  }
}
