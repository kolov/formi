package com.akolov.formi

import cats.implicits._
import com.akolov.formi.data.CvTestData
import com.akolov.formi.errors._
import com.akolov.formi.lenses._
import org.log4s.getLogger
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import com.akolov.formi.lenses.DocumentLenses._
import SingleGroupValueOps._

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

  "lens" should "insert at index 2 in field 2 to many" in {
    val innerGroup = Group("inner", List.empty, Multiplicity(2))
    val outerGroup = Group("outer", List(innerGroup), Multiplicity.Once)
    val outerGroupGroupValue: SingleGroupValue = outerGroup.singleEmpty
    val v = for {
      path <- Path.parsePath("inner[2]")
      lensInsert <- singleGroupLensFor(outerGroup, path)
      updated <- lensInsert.set(outerGroupGroupValue, innerGroup.singleEmpty)
      lensEdit <- singleGroupLensFor(outerGroup, path)
      v <- lensEdit.get(updated)
    } yield v
    v shouldEqual innerGroup.singleEmpty.asRight
  }

  "lens" should "insert new link in the Test CV template" in {
    val outerGroup = testTemplate.body
    val root: SingleGroupValue = outerGroup.singleEmpty

    root.getGroupAt(outerGroup, "Links[0]/Link[1]").isLeft shouldBe true

    val link1Updated = for {
      rootUpdated <- root.insertAt(outerGroup, "Links[0]/Link", 0)
      _ = println(s"after insert Links[0]/Link[0]: ${EntryForm.renderSingleGroup(outerGroup, rootUpdated)}")
      link1Updated <- rootUpdated.getGroupAt(outerGroup, "Links[0]/Link[1]")
    } yield link1Updated

    link1Updated shouldEqual linkGroupElement.singleEmpty.asRight

// Links[0] still has 1 link
    val links0Updated = for {
      rootUpdated <- root.insertAt(outerGroup, "Links[0]/Link", 0)
      links0Updated <- rootUpdated.getGroupAt(outerGroup, "Links[0]")
    } yield links0Updated

    links0Updated.right.get.getElement("Link").right.get shouldEqual GroupValue(
      Seq(linkGroupElement.singleEmpty, linkGroupElement.singleEmpty))
  }
}
