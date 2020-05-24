package com.akolov.formi

import com.akolov.formi.errors._
import cats.implicits._
import com.akolov.formi.lenses.Path
import com.akolov.formi.lenses.DocumentLenses._
sealed trait Value
case class GroupValue(singleGroups: Seq[SingleGroupValue]) extends Value

case class FieldValue(value: Option[String]) extends Value

case class SingleGroupValue(values: Map[String, Value]) {

  def getElement(name: String): Either[DocumentError, Value] = {
    values.find(_._1 === name) match {
      case None => PathError().asLeft
      case Some((_, v)) => v.asRight
    }
  }

  def update(name: String, newValue: Value): Either[DocumentError, SingleGroupValue] = {
    (newValue, values.get(name)) match {
      case (_, None) => PathError(s"Update failed: no such field $name").asLeft
      case (GroupValue(_), Some(GroupValue(_))) => SingleGroupValue(values.updated(name, newValue)).asRight
      case (FieldValue(_), Some(FieldValue(_))) => SingleGroupValue(values.updated(name, newValue)).asRight
      case (_, _) => BadValue().asLeft
    }
  }
}

object FieldValue {
  val Empty = FieldValue(None)

  def apply(s: String): FieldValue = new FieldValue(Some(s))
}

case class Template(name: String, body: Group) {
  def empty: SingleGroupValue = body.singleEmpty
}

trait SingleGroupValueOps {

  implicit class Operations(sgv: SingleGroupValue) {

    def insertAt(group: Group, pathString: String, at: Int): Either[DocumentError, SingleGroupValue] =
      for {
        path <- Path.parsePath(pathString)
        lens <- groupLensFor(group, path)
        currentGroup <- lens.get(sgv)
        updated = {
          val before = currentGroup.singleGroups.take(at)
          val after = currentGroup.singleGroups.drop(at)
          (before :+ currentGroup.singleGroups.head) ++ after
        }
        updated <- lens.set(sgv, GroupValue(updated))
      } yield updated

    def insertAt(group: Group, path: Path, at: Int): Either[DocumentError, SingleGroupValue] =
      for {
        lens <- groupLensFor(group, path)
        currentGroup <- lens.get(sgv)
        updated = {
          val before = currentGroup.singleGroups.take(at)
          val after = currentGroup.singleGroups.drop(at)
          (before :+ currentGroup.singleGroups.head) ++ after
        }
        updated <- lens.set(sgv, GroupValue(updated))
      } yield updated

    def getGroupAt(group: Group, pathString: String): Either[DocumentError, SingleGroupValue] =
      for {
        path <- Path.parsePath(pathString)
        lens <- singleGroupLensFor(group, path)
        r <- lens.get(sgv)
      } yield r

    def getFieldAt(group: Group, pathString: String): Either[DocumentError, FieldValue] =
      for {
        path <- Path.parsePath(pathString)
        lens <- fieldLensFor(group, path)
        f <- lens.get(sgv)
      } yield f
  }
}

object SingleGroupValueOps extends SingleGroupValueOps
