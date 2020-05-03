package com.akolov.formi

import com.akolov.formi.errors._
import cats.implicits._

sealed trait Value
case class GroupValue(singleGroups: Seq[SingleGroupValue]) extends Value

case class FieldValue(value: Option[String]) extends Value

case class SingleGroupValue(values: Map[String, Value]) {

  def get(name: String): Either[DocumentError, Value] = {
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
  def empty = body.singleEmpty
}
