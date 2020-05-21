package com.akolov.formi

import cats.implicits._
import com.akolov.formi.data.CvTestData
import com.akolov.formi.errors.DocumentError
import com.akolov.formi.lenses._
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
    val eitherLens: Either[DocumentError, DocumentLens[SingleGroupValue, SingleGroupValue]] =
      groupLensFor(outerGroup, Path(List(Indexed("inner", 0)), None))
    eitherLens.isRight shouldEqual true
    eitherLens.right.get.get(outerGroupGroupValue) shouldEqual innerGroup.singleEmpty.asRight
  }

  "lens" should "get empty value 0 from empty group 1 to 1" in {
    val innerGroup = Group("inner", List.empty, Multiplicity.Once)
    val outerGroup = Group("outer", List(innerGroup), Multiplicity.Optional)
    val outerGroupGroupValue: SingleGroupValue = outerGroup.singleEmpty
    val eitherLens: Either[DocumentError, DocumentLens[SingleGroupValue, SingleGroupValue]] =
      groupLensFor(outerGroup, Path(List(Indexed("inner", 0)), None))
    eitherLens.isRight shouldEqual true
    eitherLens.right.get.get(outerGroupGroupValue) shouldEqual innerGroup.singleEmpty.asRight
  }
  "lens" should "get empty value 1 from empty group 2 to 2" in {
    val innerGroup = Group("inner", List.empty, Multiplicity(2))
    val outerGroup = Group("outer", List(innerGroup), Multiplicity.Once)
    val outerGroupGroupValue: SingleGroupValue = outerGroup.singleEmpty
    val v = for {
      path <- Path.parsePath("inner[1]")
      lens <- groupLensFor(outerGroup, path)
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
      lens <- groupLensFor(outerGroup, path)
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
      lensInsert <- groupLensFor(outerGroup, path, Inserting)
      updated <- lensInsert.set(outerGroupGroupValue, innerGroup.singleEmpty)
      lensEdit <- groupLensFor(outerGroup, path, Editing)
      v <- lensEdit.get(updated)
    } yield v
    v shouldEqual innerGroup.singleEmpty.asRight
  }
}
