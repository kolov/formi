package com.akolov.formi

import com.akolov.formi.errors.{BadValue, DocumentError, PathError}
import cats.implicits._
// Entry in a form
trait FormEntry

case class FieldFormEntry(label: String, desc: InputDesc, value: FieldValue) extends FormEntry
case class GroupFormEntry(label: String, entries: Seq[SingleGroupFormEntry])

case class SingleGroupFormEntry(label: String, entries: Seq[FormEntry]) extends FormEntry

object EntryForm {

  def render(el: TemplateElement, value: Value): Either[DocumentError, FormEntry] = (el, value) match {
    case (f @ Field(_, _), fv @ FieldValue(_)) => renderField(f, fv)
    case (g @ Group(_, _, _), GroupValue(vals)) =>
      vals
        .map(sgv => renderSingleGroup(g, sgv))
        .sequence
        .map(vals => SingleGroupFormEntry(g.label, vals))
    case _ => BadValue().asLeft
  }
  def renderField(field: Field, fieldValue: FieldValue) = FieldFormEntry(field.label, field.desc, fieldValue).asRight

  def renderSingleGroup(group: Group, values: SingleGroupValue) = {
    val entries: Either[DocumentError, List[FormEntry]] = group.fields.map { te =>
      val v: Either[PathError, Value] =
        values.values.get(te.label).map(Right(_)).getOrElse(Left(PathError("No such name")))
      v.flatMap { vv =>
        render(te, vv)
      }
    }.sequence
    entries.map(e => SingleGroupFormEntry(group.label, e))
  }
}
