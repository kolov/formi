package com.akolov.formi

import com.akolov.formi.errors.{BadValue, DocumentError, PathError}
import cats.implicits._

case class Document(template: Group, body: SingleGroupValue)

trait View
case class FieldView(label: String, value: Option[String]) extends View
case class GroupView(label: String, entries: Seq[SingleGroupView]) extends View
case class SingleGroupView(label: String, entries: Seq[View])

object Rendered {

  def render(el: TemplateElement, value: Value): Either[DocumentError, View] = (el, value) match {
    case (f @ Field(_, _), fv @ FieldValue(_)) => renderField(f, fv)
    case (g @ Group(_, _, _), fv @ GroupValue(vals)) =>
      vals
        .map(sgv => renderSingleGroup(g, sgv))
        .toList
        .sequence
        .map(vals => GroupView(g.label, vals))
    case _ => BadValue().asLeft
  }
  def renderField(field: Field, fieldValue: FieldValue) = FieldView(field.label, fieldValue.value).asRight

  def renderSingleGroup(group: Group, singleGroupValue: SingleGroupValue): Either[DocumentError, SingleGroupView] = {
    val entries: Either[DocumentError, List[View]] = group.fields.map { te =>
      val optElement = singleGroupValue.values.get(te.label)
      optElement.map(Right(_)).getOrElse(Left(PathError("No such name"))).flatMap { vals =>
        render(te, vals)
      }
    }.sequence
    entries.map(e => SingleGroupView(group.label, e))
  }
}
