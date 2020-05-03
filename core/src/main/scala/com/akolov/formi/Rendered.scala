package com.akolov.formi

import com.akolov.formi.errors.{BadValue, DocumentError, PathError}
import cats.implicits._

case class Document(template: Group, body: SingleGroupValue)

object Rendered {
  trait Element

  case class FieldElement(label: String, value: Option[String]) extends Element
  case class GroupElement(label: String, entries: Seq[SingleGroupElement]) extends Element
  case class SingleGroupElement(label: String, entries: Seq[Element])

  def render(el: TemplateElement, value: Value): Either[DocumentError, Element] = (el, value) match {
    case (f @ Field(_, _), fv @ FieldValue(_)) => renderField(f, fv)
    case (g @ Group(_, _, _), fv @ GroupValue(vals)) =>
      vals
        .map(sgv => renderSingleGroup(g, sgv))
        .toList
        .sequence
        .map(vals => GroupElement(g.label, vals))
    case _ => BadValue().asLeft
  }
  def renderField(field: Field, fieldValue: FieldValue) = FieldElement(field.label, fieldValue.value).asRight

  def renderSingleGroup(group: Group, values: SingleGroupValue): Either[DocumentError, SingleGroupElement] = {
    val entries: Either[DocumentError, List[Element]] = group.fields.map { te =>
      values.values.get(te.label).map(Right(_)).getOrElse(Left(PathError("No such name"))).flatMap { vals =>
        render(te, vals)
      }
    }.sequence
    entries.map(e => SingleGroupElement(group.label, e))
  }
}
